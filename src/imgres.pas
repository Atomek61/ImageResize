unit imgres;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2024 Jan Schirrmacher, www.atomek.de
//
//  See https://github.com/Atomek61/ImageResize.git for licensing
//
//  imgres.pas has the processing core of both, the CLI and the GUI
//  applications. The core is called the "Processor".
//
//  The Processor can spawn a Task for each source image to be processed.
//
//  It bases heavily on the BGRABitmap library and Atomeks threading.dispatcher
//  library.
//
////////////////////////////////////////////////////////////////////////////////

{$mode Delphi}
{$modeSwitch advancedRecords}
{$modeSwitch typehelpers}

interface

uses
  Classes, SysUtils, StrUtils, Types, BGRABitmap, BGRABitmapTypes, RegExpr,
  threading.dispatcher, Tags, Logging, StringArrays, FileUtil, Templates;

resourcestring
  SCptInterpolationDefault  = 'Default';
  SCptStretch               = 'Stretch';
  SCptBox                   = 'Box';
  SCptLinear                = 'Linear';
  SCptHalfCosine            = 'Half Cosine';
  SCptCosine                = 'Cosine';
  SCptBicubic               = 'Bicubic';
  SCptMitchell              = 'Mitchell';
  SCptSpline                = 'Spline';
  SCptLanczos2              = 'Lanczos 2px Radius';
  SCptLanczos3              = 'Lanczos 3px Radius';
  SCptLanczos4              = 'Lanczos 4px Radius';
  SCptBestQuality           = 'Best Quality';

  SCptJPEGQualityDefault    = 'Default';

  SCptPNGCompressionDefault = 'Default';
  SCptPNGCompressionNone    = 'None';
  SCptPNGCompressionFastest = 'Fastest';
  SCptPNGCompressionMax     = 'Maximum';

  SCptRandomSeed            = 'Random';

  SCptSingleThread          = 'Single';
  SCptMaximumThread         = 'Maximum';

  SCptSharpenDefault        = 'Default';
  SCptSharpenNone           = 'None';
  SCptSharpenLight          = 'Light';
  SCptSharpenStrong         = 'Strong';

type
  TTagsSource = (tsEXIF, tsTagsFiles); // Tags from EXIF and/or .tags files
  TTagsSources = set of TTagsSource;
  TScalingDirection = (sdUpScaling, sdNoscaling, sdDownScaling);
  TInterpolation = (ipDefault, ipStretch, ipBox, ipLinear, ipHalfCosine, ipCosine, ipBicubic,
    ipMitchell, ipSpline, ipLanczos2, ipLanczos3, ipLanczos4, ipBestQuality);
  TTagsReport = (trTagsReport, trImgTags);
  TTagsReports = set of TTagsReport;

const
  INTERPOLATION_NAMES :array[TInterpolation] of string = ('Default',
    'Stretch', 'Box', 'Linear', 'HalfCosine', 'Cosine', 'Bicubic',
    'Mitchell', 'Spline', 'Lanczos2', 'Lanczos3', 'Lanczos4', 'BestQuality');

  INTERPOLATION_STRINGS :array[TInterpolation] of string = (SCptInterpolationDefault,
    SCptStretch, SCptBox, SCptLinear, SCptHalfCosine, SCptCosine, SCptBicubic,
    SCptMitchell, SCptSpline, SCptLanczos2, SCptLanczos3, SCptLanczos4, SCptBestQuality);

  JPEGQUALITY_NAMES :array[0..7] of string = ('Default',
    '1', '10', '25', '50', '75', '90', '100');

  JPEGQUALITY_STRINGS :array[0..7] of string = (SCptJPEGQualityDefault,
    '1', '10', '25', '50', '75', '90', '100');

  PNGCOMPRESSION_NAMES :array[0..3] of string = ('Default', 'None',
    'Fastest', 'Maximum');

  PNGCOMPRESSION_STRINGS :array[0..3] of string = (SCptPNGCompressionDefault,
    SCptPNGCompressionNone, SCptPNGCompressionFastest, SCptPNGCompressionMax);

  SHARPEN_VALUES :array[0..3] of integer = (100, 0, 50, 200);

  SHARPEN_NAMES :array[0..3] of string = ('Default', 'None', 'Light', 'Strong');

  SHARPEN_STRINGS :array[0..3] of string = (SCptSharpenDefault, SCptSharpenNone,
    SCptSharpenLight, SCptSharpenStrong);

const
  IMGRESVER = '4.2';
  IMGRESCPR = 'imgres '+IMGRESVER+' © 2024 Jan Schirrmacher, www.atomek.de';

  TAGSREPORTFILETITLE       = '.tagsreport';
  IMAGEINFOSFILETITLE       = '.imgtags';

  DEFAULTPNGCOMPRESSION     = 2;
  DEFAULTJPEGQUALITY        = 75;
  DEFAULT_INTERPOLATION     = ipLanczos2;
  DEFAULT_MRKSIZE           = 20.0;
  DEFAULT_MRKX              = 98.0;
  DEFAULT_MRKY              = 98.0;
  DEFAULT_MRKOPACITY        = 50.0;
  DEFAULT_THREADCOUNT       = 0;
  DEFAULT_STOPONERROR       = true;
  DEFAULT_RENENABLED        = false;
  DEFAULT_RENFMTSTR         = 'img%2:s.%1:s';
  DEFAULT_RENFILETEMPLATE   = 'img$(INDEX.ifmt(1)).$(FILEEXT)';
  DEFAULT_RENINDEXSTART     = 1;
  DEFAULT_RENINDEXDIGITS    = 3;
  DEFAULT_SHUFFLE           = false;
  DEFAULT_SHUFFLESEED       = 0;
  DEFAULT_SHARPEN           = 100;
  DEFAULT_FILETAGS          = nil;
  DEFAULT_COPYRIGHT         = '';
  DEFAULT_TAGSSOURCES       = [];
  DEFAULT_TAGSREPORTS       = [];
  DEFAULT_DRYRUN            = false;

type

  TSizes = array of integer;
  TSizeNames = array of string;

  TPrintEvent = procedure(Sender :TObject; const Line :string; Level :TLogLevel = llInfo) of object;
  TProgressEvent = procedure(Sender :TObject; Progress :single) of object;

  //////////////////////////////////////////////////////////////////////////////
  // Main class: resamples a list of images to a list of images and sizes

  { TProcessor }

  TProcessor = class
  private const
    IFMT_PARAMS_REGEXPR = '^(\d+)(?:,(\d+|auto))?$';
  public type

    TWatermarkParams = record
      Filename :string;
      Size    :single;
      X       :single;
      Y       :single;
      Opacity :single;
    end;

  private type

    // Needed while processing

    { TProcessorResources }

    TProcessorResources = class // Temporary process global ressources while processing
      IsMultipleTargetFolderStrategy :boolean;
      IsTargetFileRenamingStrategy :boolean;
      SourceFilenames :TStringArray; // After Shaking
      MrkImages :array of TBGRABitmap;  // Empty, One or for each Size
      TargetFolders :array of string;   // One folder for each Size
      TargetFoldersImageInfos :array of string;  // Names of the .images files, each folder has at least one
      FilesTags :TFilesTags;            // A database of all tags of all files
      TagsRequired :boolean;
      constructor Create;
      destructor Destroy; override;
    end;

    { TImageTask }

    TImageTask = class(TCustomTask) // Each image is a task
    protected
      function Execute(Context :TContext) :boolean; override;
      function GetTaskSteps :integer; override;
    public
      Processor :TProcessor;
      ProcRes :TProcessorResources;
      SourceFilename :string;
      SourceFileIndex :integer;
      FileTags :TTags;
    end;

  private // Processing Params
    FSourceFilenames  :TStrings;
    FTargetFolder     :string;
    FSizes            :TSizes;
    FSizeNames        :TSizeNames;
    FJPEGQuality      :integer;
    FPNGCompression   :integer;
    FResampleMode     :TResampleMode;
    FResampleFilter   :TResampleFilter;
    FWaterMarkParams  :TWatermarkParams;
    FThreadCount      :integer;
    FStopOnError      :boolean;
    FTargetFilename   :string; // may contain placeholders
    FShuffle          :boolean;
    FShuffleSeed      :integer;
    FSharpen          :integer; // 0 = Off
    FTagsSources      :TTagsSources;
    FTagKeys          :TStringArray; // 'Copyright',
    FCopyright        :string;
    FTagsReports      :TTagsReports;
    FDryRun           :boolean;
  private
    FActive :boolean;
    FCancelled :boolean;
    FSuccess :boolean;
    FOnPrint :TPrintEvent;
    FOnProgress :TProgressEvent;
    function GetRename: boolean;
    function GetSize: integer;
    function GetInterpolation: TInterpolation;
    function GetSizes: string;
    function GetSourceFilenames: TStrings;
    procedure SetActive(AValue: boolean);
    procedure SetSharpen(AValue: integer);
    procedure SetSize(AValue: integer);
    procedure SetInterpolation(AValue: TInterpolation);
    procedure SetSizes(AValue: string);
    procedure SetJPEGQuality(AValue: integer);
    procedure SetPNGCompression(AValue: integer);
    procedure SetWatermarkParams(const AValue: TWatermarkParams);
    procedure SetSourceFilenames(AValue: TStrings);
    procedure SetTargetFolder(AValue: string);
    procedure SetThreadCount(AValue: integer);
    procedure SetShuffleSeed(AValue :integer);
    function ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
    class function CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
    procedure OnTaskPrint(Sender :TObject; WorkerId: integer; const Line :string; Level :TLevel);
    procedure OnTaskProgress(Sender :TObject; Progress :single);
    procedure Print(const Line :string; Level :TLevel = mlInfo);
    procedure DoExecute;
    function ifmt(const Value, Params :string) :string;
    function fmt(const Value, Params :string) :string;
  public
    class var FormatSettings :TFormatSettings;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetVersion: string;
    function Execute :boolean;
    procedure Cancel;
    class function TryStrToInterpolation(const Str :string; out Value :TInterpolation) :boolean;
    class function StrToInterpolation(const Str :string) :TInterpolation;
    class function TryNameToInterpolation(const Name :string; out Value :TInterpolation) :boolean;
    class function NameToInterpolation(const Name :string) :TInterpolation;
    class function TryNameToPNGCompression(const Name :string; out Value :integer) :boolean;
    class function NameToPNGCompression(const Name :string) :integer;
    class function PNGCompressionToName(const Value :integer) :string;
    class function TryStrToJPEGQuality(const Str :string; out Value :integer) :boolean;
    class function StrToJPEGQuality(const Str :string) :integer;
    class function JPEGQualityToStr(Value :integer) :string;
    class function TryStrToSharpen(const Str :string; out Value :integer) :boolean;
    class function SharpenToStr(Value :integer) :string;
    class function StrToSharpen(const Str :string) :integer;
    class function NameToSharpen(const Name :string) :integer;
    class function SharpenToName(Value :integer) :string;
    class function TryStrToTagsSources(const Str :string; out Value :TTagsSources) :boolean;
    class function TryStrToTagsReports(const Str :string; out Value :TTagsReports) :boolean;
    class procedure StrToWatermarkParams(const Str :string; out Value :TWatermarkParams);
    property Active :boolean read FActive write SetActive;
    property Success :boolean read FSuccess;
    property SourceFilenames :TStrings read GetSourceFilenames write SetSourceFilenames;
    property TargetFolder :string read FTargetFolder write SetTargetFolder;
    property TargetFilename :string read FTargetFilename write FTargetFilename;
    property Rename :boolean read GetRename;
    property Size :integer read GetSize write SetSize;
    property Sizes :TSizes read FSizes write FSizes;
    property SizeNames :TSizeNames read FSizeNames write FSizeNames;
    property JPEGQuality :integer read FJPEGQuality write SetJPEGQuality;
    property PNGCompression :integer read FPNGCompression write SetPNGCompression;
    property Interpolation :TInterpolation read GetInterpolation write SetInterpolation;
    property WatermarkParams :TWatermarkParams read FWatermarkParams write SetWatermarkParams;
    property ThreadCount :integer read FThreadCount write SetThreadCount;
    property StopOnError :boolean read FStopOnError write FStopOnError;
    property Shuffle :boolean read FShuffle write FShuffle;
    property ShuffleSeed :integer read FShuffleSeed write SetShuffleSeed;
    property Sharpen :integer read FSharpen write SetSharpen;
    property TagsSources :TTagsSources read FTagsSources write FTagsSources;
    property TagKeys :TStringArray read FTagKeys write FTagKeys;
    property Copyright :string read FCopyright write FCopyright;
    property TagsReports :TTagsReports read FTagsReports write FTagsReports;
    property DryRun :boolean read FDryRun write FDryRun;
    property OnPrint :TPrintEvent read FOnPrint write FOnPrint;
    property OnProgress :TProgressEvent read FOnProgress write FOnProgress;
  end;

function TryStrToSizes(const Str :string; out Values :TSizes) :boolean;
function StrToSizes(const Str :string) :TSizes;
function SizesToStr(const Sizes :TSizes) :string;

resourcestring
  SCptHint                        = 'Hint';
  SCptWarning                     = 'Warning';
  SCptAbort                       = 'Abort';
  SCptFatal                       = 'Fatal';
  SMsgWarningDryRun               = 'Not really creating any image (-dryrun flag)';
  SErrFormatNotSupportedFmt       = 'Format %s not supported';
  SMsgDownScalingFmt              = 'Down-scaling ''%s'' from %dx%d to %dx%d...';
  SMsgUpScalingFmt                = 'Up-scaling ''%s'' from %dx%d to %dx%d...';
  SMsgNoScalingFmt                = 'No scaling ''%s'' (%dx%d to %dx%d)...';
  SMsgWatermarkingFmt             = 'Watermarking ''%s''...';
  SMsgSavingFmt                   = 'Saving ''%s''...';
  SMsgLoadingFmt                  = 'Loading ''%s''...';
  SMsgLoadingTags                 = 'Loading .tags...';
  SMsgWritingTagsReportFmt        = 'Writing Tags Report to ''%s''...';
  SMsgReadingExifFmt              = 'Reading EXIF from ''%s''...';
  SMsgCreatingFolderFmt           = 'Creating folder ''%s''...';
  SMsgDeletingImageInfosFmt       = 'Deleting file ''%s''...';
  SMsgShakingFiles                = 'Shaking list of files...';
  SMsgLoadMrkFileFmt              = 'Loading Watermark ''%s''...';
  SMsgWritingExifFmt              = 'Writing EXIF to ''%s''...';
  SErrInvalidThreadCount          = 'Invalid threadcount';
  SErrMissingSizes                = 'Missing sizes.';
  SErrInvalidSizesFmt             = 'Invalid sizes ''%s''';
  SErrMultipleSizes               = 'Multiple sizes but placeholder SIZE or SIZENAME not found in either folder or filename template';
  SErrInvalidPNGCompressionFmt    = 'Invalid PNG compression %d (0..3 expected)';
  SErrInvalidPNGCompressionNameFmt= 'Invalid PNG compression ''%s'' (Default, None, Fastest or Maximum expected)';
  SErrInvalidJPEGQualityFmt       = 'Invalid JPEG quality ''%s'' (1..100 expected)';
  SErrInvalidSharpenNameFmt       = 'Invalid sharpen value ''%s'' (Default, None, Light, Strong or any integer >= 0 expected)';
  SErrInvalidSharpenStringFmt     = 'Invalid sharpening amount ''%d'', 0..n expected';
  SErrInvalidSharpenStrFmt        = 'Invalid sharpening amount ''%s'', 0..n expected';
  SErrInvalidRenamingParamFmt     = 'Invalid renaming parameter ''%s''';
  SErrInvalidINDEXPlaceholderFmt  = 'Invalid INDEX placeholder parameters ''%s''';
  SErrInvalidINDEXStartFmt        = 'Invalid INDEX start ''%s''';
  SErrInvalidINDEXDigitsFmt       = 'Invalid INDEX digits number ''%s''';
  SErrInvalidINDEXParamCountFmt   = 'Invalid INDEX parameter count ''%s'' (2 expected)';
  SErrInvalidPlaceholder          = 'Unknown or invalid placeholder';
  SInfResultFmt                   = 'Images: %d, Filter: %s, Sizes: %d, Tasks: %d, Successful: %d, Failed: %d, Elapsed: %.2fs';
  SErrInvalidInterpolationFmt     = 'Invalid interpolation name ''%s''';
  SErrInvalidMrkParamCountFmt     = 'Invalid number of watermark parameters ''%s'' (1, 2 or 3 expected)';
  SErrInvalidMrkFloatCountFmt     = 'Invalid watermark size/pos parameters ''%s'' (3 comma-separated floats expected)';
  SErrInvalidMrkFloatRangeFmt     = 'Invalid watermark size/position range ''%s'' (0..100.0 expected)';
  SErrInvalidMrkOpacityFmt        = 'Invalid watermark opacity value ''%s'' (0..100.0 expected)';
  SErrIfmtParamsFmt               = 'invalid parameters (offset[,digits]) expected';
  SErrFmtParamsFmt                = 'invalid parameter (digits|AUTO|<none>) expected';
  SErrInExpected                  = 'integer expected';

implementation

uses
  Math, ZStream, FPWriteJpeg, FPWritePng, FPImage, Utils,
  generics.collections, EXIFUtils;

const
  TAGID_COPYRIGHT = 'Copyright';

const
  LEVELSTRS :array[TLevel] of string = (SCptHint, '', SCptWarning, SCptAbort, SCptFatal);

type
  TIntegerArrayHelper = TArrayHelper<Integer>;

function GetParentDirectory(const Directory :string) :string;
var
  i, n :integer;
begin
  n := Length(Directory);
  if (n>0) and (Directory[n]=DirectorySeparator) then
    dec(n);
  for i:=n downto 1 do
    if Directory[i]=DirectorySeparator then
      Exit(Copy(Directory, 1, i));
  result := '';
end;

function TryStrToSizes(const Str :string; out Values :TSizes) :boolean;
var
  Raw :TIntegerDynArray;
  i, n :integer;
begin
  if not StrToIntegerArray(Str, ',', Raw) then Exit(false);
  for i:=0 to High(Raw) do if Raw[i]<1 then Exit(false);

  // Sort...
  TIntegerArrayHelper.Sort(Raw);

  // Remove doublettes
  if Length(Raw)>0 then begin
    n := 1;
    Values := nil;
    SetLength(Values, 1);
    Values[0] := Raw[0];
    for i:=1 to High(Raw) do begin
      if Raw[i]<>Raw[i-1] then begin
        SetLength(Values, n+1);
        Values[n] := Raw[i];
        inc(n);
       end;
    end;
  end;

  result := true;
end;

function StrToSizes(const Str: string): TSizes;
begin
  if not TryStrToSizes(Str, result) then
    raise Exception.CreateFmt(SErrInvalidSizesFmt, [Str]);
end;

function SizesToStr(const Sizes :TSizes) :string;
var
  i :integer;
begin
  if Length(Sizes)=0 then
    Exit('');
  result := IntToStr(Sizes[0]);
  for i:=1 to High(Sizes) do
    result := result + ', ' + IntToStr(Sizes[i]);
end;

function TryJPEGQualityNameToIndex(const Name :string; out Index :integer) :boolean;
var
  i :integer;
begin
  for i:=0 to High(JPEGQUALITY_NAMES) do
    if SameText(JPEGQUALITY_NAMES[i], Name) then begin
      Index := i;
      Exit(true);
    end;
  result := false;
end;

{ TProcessor.TProcessorResources }

constructor TProcessor.TProcessorResources.Create;
begin
  FilesTags := TFilesTags.Create;
end;

destructor TProcessor.TProcessorResources.Destroy;
var
  i :integer;
begin
  FilesTags.Free;
  for i:=0 to High(MrkImages) do
    MrkImages[i].Free;
  inherited;
end;

{ TProcessor.TImageTask }

function TProcessor.TImageTask.Execute(Context: TContext): boolean;
var
  SourceImg :TBGRABitmap;
  TargetFolder :string;
  TargetFileExt :string;
  TargetFiletitle :string;
  TargetFiletitleExt :string;
  TargetFilenameTemplateEngine :Templates.TEngine;
  TargetFilename :string;
  TargetImg :TBGRABitmap;
  SharpImg :TBGRABitmap;
  Writer :TFPCustomImageWriter;
  Size :integer;
  SourceSize :TSize;
  TargetSize :TSize;
  MrkImg :TBGRABitmap;
  MrkRectSize :TSize;
  MrkRect :TRect;
  SizeStr :string;
  i, m :integer;
  ExifTags :TTags;
  ExifTagKeys :TStringArray;
  TagKey :string;
  MsgScalingFmt :string;
  MsgScalingLevel :TLevel;
begin

  result := false;
  if Context.Cancelled then
    Exit;

  SourceImg := nil;
  TargetFilenameTemplateEngine := nil;

  // Source File
  try
    if Context.Cancelled then Exit;

    // Load source file...
    Print(Format(SMsgLoadingFmt, [SourceFilename]));
    SourceImg := TBGRABitmap.Create(SourceFilename);
    SourceSize := TSize.Create(SourceImg.Width, SourceImg.Height);

    if Context.Cancelled then Exit;
    Progress(1);

    if (tsEXIF in Processor.FTagsSources) and IsJPEG(SourceFilename) then begin
      // Add EXIF tags to FileTags
      Print(Format(SMsgReadingExifFmt, [SourceFilename]));
      ExifTags := TTags.Create;
      try
        ReadExifTags(SourceFilename, ExifTags, ExifTagKeys);
        for TagKey in ExifTagKeys do
          if not FileTags.ContainsKey(TagKey) then
            FileTags.AddOrSetValue(TagKey, ExifTags[TagKey]);
      finally
        ExifTags.Free;
      end;
    end;

    if Processor.FCopyright<>'' then
      FileTags.AddOrSetValue(TAGID_COPYRIGHT, Processor.FCopyright);

    // If a report will be written, then add tags
    if ProcRes.TagsRequired then begin
      FileTags.AddOrSetValue('OrgWidth', IntToStr(SourceSize.cx));
      FileTags.AddOrSetValue('OrgHeight', IntToStr(SourceSize.cy));
      FileTags.AddOrSetValue('OrgFilename', ExtractFilename(SourceFilename));
      FileTags.AddOrSetValue('OrgFilesize', IntToStr(FileUtil.FileSize(SourceFilename)));
    end;

    if Processor.Rename then begin
      TargetFilenameTemplateEngine := Templates.TEngine.Create;
      TargetFilenameTemplateEngine.RegisterFunction('ifmt', Processor.ifmt);
      TargetFilenameTemplateEngine.RegisterFunction('fmt', Processor.fmt);
    end;

    ////////////////////////////////////////////////////
    // Big SIZE loop
    m := Length(Processor.FSizes);
    for i:=0 to m-1 do begin

      Writer := nil;
      TargetImg := nil;

      try
        // Create Writer depending on file extension
        TargetFileExt := ExtractExt(SourceFilename);
        if IsJPEG(SourceFilename) then begin

          if not Processor.FDryRun then begin
            // Jpg-options
            Writer := TFPWriterJPEG.Create;
            with TFPWriterJPEG(Writer) do
              CompressionQuality := TFPJPEGCompressionQuality(Processor.JPEGQuality);
          end;

        end else if IsPNG(SourceFilename) then begin

          if not Processor.FDryRun then begin
            // Png-options
            Writer := TFPWriterPNG.Create;
            with TFPWriterPNG(Writer) do
              CompressionLevel := ZStream.TCompressionLevel(Processor.PNGCompression);
          end;

        end else
          raise Exception.CreateFmt(SErrFormatNotSupportedFmt, [TargetFileExt]);

        // Calculate new size
        Size := Processor.FSizes[i];
        TargetSize := CalcResamplingSize(SourceSize, Size);

        // Scaling direction
        if (SourceSize.cx=TargetSize.cx) and (TargetSize.cy=TargetSize.cy) then begin
          MsgScalingFmt := SMsgNoScalingFmt;
          MsgScalingLevel := mlWarning;
        end else if (SourceSize.cx<TargetSize.cx) or (TargetSize.cy<TargetSize.cy) then begin
          MsgScalingFmt := SMsgUpScalingFmt;
          MsgScalingLevel := mlWarning;
        end else begin
          MsgScalingFmt := SMsgDownScalingFmt;
          MsgScalingLevel := mlInfo;
        end;

        ////////////////////////////////////////////////////////////////////////////
        // Resampling...
        Print(Format(MsgScalingFmt, [
          ExtractFilename(SourceFilename), SourceSize.cx, SourceSize.cy, TargetSize.cx, TargetSize.cy]), MsgScalingLevel);
        TargetImg := Processor.ResampleImg(SourceImg, TargetSize);

        // Sharpen...
        if Processor.FSharpen>0 then begin
          SharpImg := TargetImg.FilterSharpen(Processor.FSharpen/100.0);
          TargetImg.Free;
          TargetImg := SharpImg;
        end;

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        ////////////////////////////////////////////////////////////////////////////
        // Watermark
        if Processor.FWatermarkParams.Filename<>'' then begin
          MrkImg := ProcRes.MrkImages[i mod Length(ProcRes.MrkImages)];

          // Watermark size in percent of the width or original size if MrkSize=0.0
          if Processor.FWatermarkParams.Size<>0.0 then begin
            MrkRectSize.cx := round(TargetSize.cx*Processor.FWatermarkParams.Size/100.0);
            MrkRectSize.cy := round(TargetSize.cx*Processor.FWatermarkParams.Size/100.0 * MrkImg.Height/MrkImg.Width);
          end else begin
            MrkRectSize.cx := MrkImg.Width;
            MrkRectSize.cy := MrkImg.Height;
          end;
          MrkRect.Left := round((TargetSize.cx - MrkRectSize.cx) * Processor.FWatermarkParams.X/100.0);
          MrkRect.Top := round((TargetSize.cy - MrkRectSize.cy) * Processor.FWatermarkParams.Y/100.0);
          MrkRect.Width := MrkRectSize.cx;
          MrkRect.Height := MrkRectSize.cy;
          Print(Format(SMsgWatermarkingFmt, [ExtractFilename(SourceFilename), SourceSize.cx, SourceSize.cy, TargetSize.cx, TargetSize.cy]));
          TargetImg.StretchPutImage(MrkRect, MrkImg, dmLinearBlend, round(255*(Processor.FWatermarkParams.Opacity/100.0)));
        end;

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        // Compile TargetFilename
        if Processor.Rename then begin
          TargetFiletitle := ExtractFilename(SourceFilename);
          if Length(TargetFileExt)>0 then
            TargetFiletitle := Copy(TargetFiletitle, 1, Length(TargetFiletitle)-Length(TargetFileExt)-1);
          TargetFilenameTemplateEngine.Load('FILENAME', ExtractFilename(SourceFilename));
          TargetFilenameTemplateEngine.Load('FILETITLE', TargetFiletitle);
          TargetFilenameTemplateEngine.Load('FILEEXT', TargetFileExt);
          TargetFilenameTemplateEngine.Load('INDEX', IntToStr(SourceFileIndex));
          TargetFilenameTemplateEngine.Load('SIZE', IntToStr(Size));
          TargetFilenameTemplateEngine.Load('SIZENAME', ifthen(Processor.SizeNames[i]='', IntToStr(Size), Processor.SizeNames[i]));
          TargetFilenameTemplateEngine.Load('INTERPOLATION', INTERPOLATION_NAMES[Processor.Interpolation]);
          TargetFiletitleExt := TargetFilenameTemplateEngine.Compile(Processor.TargetFilename);
        end else
          TargetFiletitleExt := ExtractFilename(SourceFilename);

        // Save
        TargetFolder := ProcRes.TargetFolders[i mod Length(ProcRes.TargetFolders)];
        TargetFilename := IncludeTrailingPathDelimiter(TargetFolder) + TargetFiletitleExt;
        Print(Format(SMsgSavingFmt, [TargetFilename]));
        if not Processor.FDryRun then
          TargetImg.SaveToFile(TargetFilename, Writer);

        // Add some size dependent un-normalized information.
        // Later the Process.Execute method will extract this information and
        // writes an .imgtags file for each size
        if ProcRes.TagsRequired then begin
          SizeStr := IntToStr(Size)+'.';
          FileTags.Add(SizeStr+'ImgFilename', ExtractFilename(TargetFilename));
          FileTags.Add(SizeStr+'ImgWidth', IntToStr(TargetSize.cx));
          FileTags.Add(SizeStr+'ImgHeight', IntToStr(TargetSize.cy));
          FileTags.Add(SizeStr+'ImgFilesize', IntToStr(FileUtil.FileSize(TargetFilename)));
        end;
      finally
        Writer.Free;
        TargetImg.Free;
      end;

      // EXIF
      if (Length(Processor.FTagKeys)>0) and IsJPEG(TargetFilename) then begin
        Print(Format(SMsgWritingExifFmt, [TargetFilename]));
        if not Processor.FDryRun then
          WriteExifTags(TargetFilename, FileTags, Processor.FTagKeys);
      end;

      Progress(1);

    end;
    result := true;
  finally
    TargetFilenameTemplateEngine.Free;
    SourceImg.Free;
  end;
end;

function TProcessor.TImageTask.GetTaskSteps: integer;
begin
  result := 1 + 3*Length(Processor.FSizes);
end;

{ TProcessor }

constructor TProcessor.Create;
begin
  FSourceFilenames  := TStringList.Create;
  FSizes            := nil;
  FJPEGQuality      := DEFAULTJPEGQUALITY;
  FPNGCompression   := DEFAULTPNGCOMPRESSION;
  Interpolation     := DEFAULT_INTERPOLATION;
  with FWatermarkParams do begin
    Filename  := '';
    Size      := DEFAULT_MRKSIZE;
    X         := DEFAULT_MRKX;
    Y         := DEFAULT_MRKY;
    Opacity   := DEFAULT_MRKOPACITY;
  end;
  FThreadCount      := DEFAULT_THREADCOUNT;
  FStopOnError      := DEFAULT_STOPONERROR;
  FTargetFilename   := '';
  FShuffle          := DEFAULT_SHUFFLE;
  FShuffleSeed      := DEFAULT_SHUFFLESEED;
  FSharpen          := DEFAULT_SHARPEN;
  FTagsSources      := DEFAULT_TAGSSOURCES;
  FTagKeys          := nil;
  FCopyright        := DEFAULT_COPYRIGHT;
  FTagsReports      := DEFAULT_TAGSREPORTS;
  FDryRun           := DEFAULT_DRYRUN;
end;

destructor TProcessor.Destroy;
begin
  FSourceFilenames.Free;
  inherited Destroy;
end;

class function TProcessor.CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
var
  f :single;
begin
  f := Size.cx/Size.cy;
  if f>=1.0 then begin
    result.cx := LongWidth;
    result.cy := round(LongWidth/f);
  end else begin
    result.cy := LongWidth;
    result.cx := round(LongWidth*f);
  end;
end;

procedure TProcessor.Print(const Line: string; Level: TLevel);
begin
  if Assigned(FOnPrint) then
    OnTaskPrint(self, -1, Line, Level);
end;

procedure TProcessor.OnTaskPrint(Sender: TObject; WorkerId: integer; const Line: string; Level: TLevel);
var
  Prefix :string;
const
  LOGLEVELS :array[TLevel] of TLogLevel = (llHint, llInfo, llWarning, llError, llCrash);
begin
  if Assigned(FOnPrint) then begin
    Prefix := LEVELSTRS[Level];
    if Prefix<>'' then
      Prefix := Prefix + '  ';
    FOnPrint(self, Format('[%d] %s%s', [WorkerId+1, Prefix, Line]), LOGLEVELS[Level]);
  end;
end;

procedure TProcessor.OnTaskProgress(Sender: TObject; Progress: single);
begin
  if Assigned(FOnProgress) then begin
    FOnProgress(self, Progress);
    if FCancelled then
      (Sender as TDispatcher).Cancel;
  end;
end;

procedure TProcessor.SetThreadCount(AValue: integer);
begin
  if FThreadCount = AValue then Exit;
  if AValue<-1 then
    raise Exception.Create(SErrInvalidThreadCount);
  FThreadCount := AValue;
end;

procedure TProcessor.SetShuffleSeed(AValue: integer);
begin
  if AValue<0 then AValue := 0;
  FShuffleSeed := AValue;
end;

function TProcessor.ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
begin
  Img.ResampleFilter := FResampleFilter;
  result := Img.Resample(Size.cx, Size.cy, FResampleMode) as TBGRABitmap;
end;

procedure TProcessor.DoExecute;
var
  ProcRes :TProcessorResources;
  Task :TImageTask;
  Tasks :TTasks;
  Dispatcher :TDispatcher;
  TargetFolderTemplateEngine :templates.TEngine;
  i, j, n, m :integer;
  Item :string;
  TagsFilename :string;
  ImgTagsFilename :string;
begin
  FActive     := false;
  FCancelled  := false;
  FSuccess    := true;

  // Check main parameters
  if FSourceFilenames.Count=0 then
    Exit;

  if Length(FSizes)=0 then
    raise Exception.Create(SErrMissingSizes);

  ProcRes     := TProcessorResources.Create;
  Tasks       := TTasks.Create;
  Dispatcher  := TDispatcher.Create;
  TargetFolderTemplateEngine := templates.TEngine.Create;
  try

    if FDryRun then
      Print(SMsgWarningDryRun, mlWarning);

    // Build list of SourceFilenames with absolute paths
    n := FSourceFilenames.Count;
    SetLength(ProcRes.SourceFilenames, n);
    for i:=0 to n-1 do
      ProcRes.SourceFilenames[i] := ExpandFilename(FSourceFilenames[i]);

    // If the files list has to be shuffled
    if FShuffle then begin
      Print(SMsgShakingFiles);
      if FShuffleSeed=0 then Randomize else RandSeed := FShuffleSeed;
      for i:=0 to n-1 do begin
        j := Random(n);
        Item := ProcRes.SourceFilenames[i];
        ProcRes.SourceFilenames[i] := ProcRes.SourceFilenames[j];
        ProcRes.SourceFilenames[j] := Item;
      end;
    end;

    // A Copyright overwrite implicitely requires tagging
    if (FCopyright<>'') and not FTagKeys.contains(TAGID_COPYRIGHT) then
      FTagKeys.Add(TAGID_COPYRIGHT);

    // Check if TagsSources are implicite
    if (FTagsSources=[]) and ((Length(FTagKeys)>0) or (trImgTags in FTagsReports)) then begin
      include(FTagsSources, tsTagsFiles);
    end;

    // Check if the tags database is required
    ProcRes.TagsRequired := (FTagsSources<>[]) or (Length(FTagKeys)>0);
    if ProcRes.TagsRequired then begin
      ProcRes.FilesTags.Prepare(ProcRes.SourceFilenames);
      if tsTagsFiles in FTagsSources then begin
        // Load from .tags
        Print(SMsgLoadingTags);
        ProcRes.FilesTags.LoadFromTags;
      end;
    end;

    // Check, if Sizenames are prepared, if not - create them
    SetLength(FSizeNames, Length(FSizes));

    // Prepare the destination file renaming feature
    m := Length(FSizes);

    // Check, if multiple sizes, then either SIZE or SIZENAME must be in folder or in renamed filename
    ProcRes.IsTargetFileRenamingStrategy := Rename and TargetFolderTemplateEngine.ContainsOneOf(FTargetFilename, ['SIZE', 'SIZENAME'], DOSDELIMITERS);
    ProcRes.IsMultipleTargetFolderStrategy := TargetFolderTemplateEngine.ContainsOneOf(FTargetFolder, ['SIZE', 'SIZENAME'], DOSDELIMITERS);
    if (Length(FSizes)>1) and not ProcRes.IsMultipleTargetFolderStrategy and not ProcRes.IsTargetFileRenamingStrategy then
        raise Exception.Create(SErrMultipleSizes);

    FActive := true;

    // Create Destination Folders
    SetLength(ProcRes.TargetFolders, m);
    if ProcRes.IsMultipleTargetFolderStrategy then begin
      for i:=0 to m-1 do begin
        TargetFolderTemplateEngine.Load('SIZE', IntToStr(FSizes[i]));
        TargetFolderTemplateEngine.Load('SIZENAME', ifthen(FSizeNames[i]='', IntToStr(FSizes[i]), FSizeNames[i]));
        ProcRes.TargetFolders[i] := IncludeTrailingPathDelimiter(ExpandFilename(TargetFolderTemplateEngine.Compile(FTargetFolder)));
        TargetFolderTemplateEngine.Clear;
      end;
    end else
      for i:=0 to m-1 do
        ProcRes.TargetFolders[i] := IncludeTrailingPathDelimiter(ExpandFilename(FTargetFolder));
    for i:=0 to m-1 do begin
      Print(Format(SMsgCreatingFolderFmt, [ProcRes.TargetFolders[i]]));
      if not FDryRun then
        ForceDirectories(ProcRes.TargetFolders[i]);
    end;

    // Delete .imgtags
    // Depending on the Strategies IsMultipleTargetFolders and IsTargetFileRenamingStrategy
    // there a 3 tactics of storing .images
    for i:=0 to m-1 do begin
      if ProcRes.IsMultipleTargetFolderStrategy or not ProcRes.IsTargetFileRenamingStrategy then
        ImgTagsFilename := ProcRes.TargetFolders[i]+IMAGEINFOSFILETITLE
      else
        ImgTagsFilename := ProcRes.TargetFolders[i]+IMAGEINFOSFILETITLE+IntToStr(FSizes[i]);
      if FileExists(ImgTagsFilename) then begin
        Print(Format(SMsgDeletingImageInfosFmt, [ImgTagsFilename]));
        DeleteFile(ImgTagsFilename);
      end;
    end;

    // Load MrkImages...
    if FWatermarkParams.Filename='' then begin
      SetLength(ProcRes.MrkImages, 0);
    end else begin
      if Pos('$(SIZE)', FWatermarkParams.Filename)>0 then begin
        SetLength(ProcRes.MrkImages, m);
        for i:=0 to m-1 do begin
          Item := ReplaceStr(FWatermarkParams.Filename, '$(SIZE)', IntToStr(FSizes[i]));
          Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(Item)]));
          ProcRes.MrkImages[i] := TBGRABitmap.Create(Item);
        end;
      end else begin
        SetLength(ProcRes.MrkImages, 1);
        Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(FWatermarkParams.Filename)]));
        ProcRes.MrkImages[0] := TBGRABitmap.Create(FWatermarkParams.Filename);
      end;
    end;

    // For each SourceFilename create a task and prepare the tasks parameters
    Tasks.Capacity := n;
    for i:=0 to n-1 do begin
      Task := TImageTask.Create;
      Task.Processor := self;
      Task.ProcRes := ProcRes;
      Task.SourceFilename := ProcRes.SourceFilenames[i];
      Task.SourceFileIndex := i;
      if ProcRes.TagsRequired then
        ProcRes.FilesTags.TryGetValue(Task.SourceFilename, Task.FileTags);
      Tasks.Add(Task);
    end;

    Dispatcher.OnPrint := OnTaskPrint;
    Dispatcher.OnProgress := OnTaskProgress;
    Dispatcher.MaxWorkerCount := ThreadCount;
    Dispatcher.StopOnError := StopOnError;

    FSuccess := Dispatcher.Execute(Tasks);
    if FSuccess then begin
      if trTagsReport in FTagsReports then begin
        if ProcRes.IsMultipleTargetFolderStrategy then
          TagsFilename := GetParentDirectory(FTargetFolder)+TAGSREPORTFILETITLE
        else
          TagsFilename := TargetFolder+TAGSREPORTFILETITLE;
        Print(Format(SMsgWritingTagsReportFmt, [TagsFilename]));
        ForceDirectories(ExtractFilePath(TagsFilename));
        ProcRes.FilesTags.SaveToFile(TagsFilename, ProcRes.FilesTags.TagKeys, [soRelative]);
      end;

      // Save .imgtags
      if trImgTags in FTagsReports then begin
        for i:=0 to m-1 do begin
          //if ProcRes.IsMultipleTargetFolderStrategy and not ProcRes.IsTargetFileRenamingStrategy then
          //  ImgTagsFilename := ProcRes.TargetFolders[i]+IMAGEINFOSFILETITLE
          //else
          ImgTagsFilename := ProcRes.TargetFolders[i]+IMAGEINFOSFILETITLE+IntToStr(FSizes[i]);
          Print(Format(SMsgWritingTagsReportFmt, [ImgTagsFilename]));
          ForceDirectories(ExtractFilePath(ImgTagsFilename));
          ProcRes.FilesTags.SaveAllToImgTagsFile(ImgTagsFilename, FSizes[i]);
        end;
      end;
    end;

    if Assigned(FOnPrint) then with Dispatcher.Stats do
      FOnPrint(self, Format(SInfResultFmt, [n, INTERPOLATION_STRINGS[Interpolation], m, TaskCount, Successful, Failed, Elapsed/1000.0]), llNews);

  finally
    Dispatcher.Free;
    Tasks.Free;
    ProcRes.Free;
    TargetFolderTemplateEngine.Free;
    FActive := false;
  end;
end;

function TProcessor.ifmt(const Value, Params: string): string;
var
  r :TRegExpr;
  Offset :integer;
  Digits :integer;
begin
  r := TRegExpr.Create(IFMT_PARAMS_REGEXPR);
  r.ModifierI := true;
  try
    if not r.Exec(Params) then
      raise Exception.CreateFmt(SErrIfmtParamsFmt, [Params]);
    Offset := StrToInt(r.Match[1]);
    if (r.Match[2]='') or SameText(r.Match[2], 'AUTO') then
      Digits := round(log10(self.FSourceFilenames.Count))+1
    else
      Digits := StrToInt(r.Match[2]);
    result := Format('%*.*d', [Digits, Digits, StrToInt(Value)+Offset])
  finally
    r.Free;
  end;
end;

function TProcessor.fmt(const Value, Params: string): string;
var
  r :TRegExpr;
  Digits :integer;
  Int :integer;
begin
  if not TryStrToInt(Value, Int) then
    raise Exception.Create(SErrInExpected);
  if Params='' then
    Digits := -1
  else begin
    r := TRegExpr.Create('^\d+|auto$');
    r.ModifierI := true;
    try
      if not r.Exec(Params) then
        raise Exception.CreateFmt(SErrFmtParamsFmt, [Params]);
      if SameText(r.Match[0], 'AUTO') then
        Digits := -1
      else
        Digits := StrToInt(r.Match[0]);
    finally
      r.Free;
    end;
  end;
  if Digits<0 then
    Digits := round(log10(self.FSourceFilenames.Count))+1;
  result := Format('%*.*d', [Digits, Digits, Int]);
end;

procedure TProcessor.Cancel;
begin
  FCancelled := true;
end;

class function TProcessor.TryNameToPNGCompression(const Name: string; out Value: integer): boolean;
var
  i :integer;
begin
  for i:=0 to High(PNGCOMPRESSION_NAMES) do
    if SameText(PNGCOMPRESSION_NAMES[i], Name) then begin
        Value := i;
        Exit(true);
    end;
  result := false;
end;

class function TProcessor.NameToPNGCompression(const Name: string): integer;
begin
  if not TryNameToPNGCompression(Name, result) then
    raise Exception.CreateFmt(SErrInvalidPNGCompressionNameFmt, [Name]);

end;

class function TProcessor.PNGCompressionToName(const Value :integer) :string;
begin
  if (Value<0) or (Value>High(PNGCOMPRESSION_NAMES)) then
    raise Exception.CreateFmt(SErrInvalidPNGCompressionFmt, [Value]);
  result := PNGCOMPRESSION_NAMES[Value];
end;

class function TProcessor.TryStrToJPEGQuality(const Str :string; out Value :integer) :boolean;
begin
  result := TryStrToInt(Str, Value);
  if not result then begin
    Value := DEFAULTJPEGQUALITY;
    result := true;
  end else begin
    if Value<1 then Value := 1;
    if Value>100 then Value := 100;
  end;
end;

class function TProcessor.StrToJPEGQuality(const Str: string): integer;
begin
  if not TryStrToJPEGQuality(Str, result) then
    raise Exception.CreateFmt(SErrInvalidJPEGQUalityFmt, [Str]);
end;

class function TProcessor.JPEGQualityToStr(Value :integer) :string;
begin
  if Value<1 then Value := 1;
  if Value>100 then Value := 100;
  if Value=DEFAULTJPEGQUALITY then
    result := JPEGQUALITY_NAMES[0]
  else
    result := IntToStr(Value);
end;

class function TProcessor.TryStrToSharpen(const Str: string; out Value: integer): boolean;
var
  i :integer;
begin
  for i:=0 to High(SHARPEN_STRINGS) do
    if SameText(SHARPEN_STRINGS[i], Str) then begin
      Value := SHARPEN_VALUES[i];
      Exit(true);
    end;
  result := TryStrToInt(Str, Value) and (Value>=0);
end;

class function TProcessor.NameToSharpen(const Name: string): integer;
var
  i :integer;
begin
  for i:=0 to High(SHARPEN_NAMES) do
    if SameText(SHARPEN_NAMES[i], Name) then
      Exit(SHARPEN_VALUES[i]);
  if not TryStrToInt(Name, result) and (result>=0) then
    raise Exception.CreateFmt(SErrInvalidSharpenNameFmt, [Name]);
end;

class function TProcessor.SharpenToName(Value :integer): string;
var
  i :integer;
begin
  for i:=0 to High(SHARPEN_VALUES) do
    if SHARPEN_VALUES[i]=Value then
      Exit(SHARPEN_NAMES[i]);
  result := IntToStr(Value);
end;

class function TProcessor.SharpenToStr(Value: integer): string;
var
  i :integer;
begin
  for i:=0 to High(SHARPEN_NAMES) do
    if SHARPEN_VALUES[i]=Value then begin
      Exit(SHARPEN_STRINGS[i]);
    end;
  result := IntToStr(Value);
end;

class function TProcessor.StrToSharpen(const Str: string): integer;
begin
  if not TryStrToSharpen(Str, result) then
    raise Exception.CreateFmt(SErrInvalidSharpenStrFmt, [Str]);
end;

class function TProcessor.TryStrToTagsSources(const Str :string; out Value :TTagsSources) :boolean;
var
  Item :string;
  Items :TStringArray;
begin
  Value := [];
  Items := StrToStringArray(Str, ',');
  for Item in Items do
    if SameText(Item, 'EXIF') then include(Value, tsEXIF)
    else if SameText(Item, 'TAGS') then include(Value, tsTagsFiles)
    else Exit(false);
  result := true;
end;

class function TProcessor.TryStrToTagsReports(const Str: string; out
  Value: TTagsReports): boolean;
var
  Item :string;
  Items :TStringArray;
begin
  Value := [];
  Items := StrToStringArray(Str, ',');
  for Item in Items do
    if SameText(Item, 'TAGSREPORT') then include(Value, trTagsReport)
    else if SameText(Item, 'IMGTAGS') then include(Value, trImgTags)
    else Exit(false);
  result := true;
end;

// "path\mark[{SIZE}].png[:size,x,y[:opacity]]"   count: 1, 2, 3 allowed
class procedure TProcessor.StrToWatermarkParams(const Str: string; out Value: TWatermarkParams);
var
  Items :TStringArray;
  Floats :TSingleDynArray;
  n :integer;
begin
  Floats := nil;
  Items := Str.Split(':');
  n := Length(Items);
  if (n<1) or (n>3) then
    raise Exception.CreateFmt(SErrInvalidMrkParamCountFmt, [Str]);
  Value.Filename  := Items[0];
  Value.Size      := 20.0;
  Value.X         := 98.0;
  Value.Y         := 98.0;
  Value.Opacity   := 50.0;
  if n>1 then begin
    if not StrToSingleArray(Items[1], ',', Floats, FormatSettings) or (Length(Floats)<>3) then
      raise Exception.CreateFmt(SErrInvalidMrkFloatCountFmt, [Items[1]]);
    if (Floats[0]<0.0) or (Floats[0]>100.0) or (Floats[1]<0.0) or (Floats[1]>100.0) or (Floats[2]<0.0) or (Floats[2]>100.0) then
      raise Exception.CreateFmt(SErrInvalidMrkFloatRangeFmt, [Items[1]]);
    Value.Size := Floats[0];
    Value.X := Floats[1];
    Value.Y := Floats[2];
  end;
  if (n>2) and not (TryStrToFloat(Items[2], Value.Opacity, FormatSettings) and (Value.Opacity>=0.0) and (Value.Opacity<=100.0)) then
    raise Exception.CreateFmt(SErrInvalidMrkOpacityFmt, [Items[2]]);
end;

class function TProcessor.TryStrToInterpolation(const Str: string; out Value: TInterpolation): boolean;
var
  i :TInterpolation;
begin
  for i:=Low(TInterpolation) to High(TInterpolation) do
    if SameText(Str, INTERPOLATION_STRINGS[i]) then begin
      Value := i;
      Exit(true);
    end;
  result := false;
end;

class function TProcessor.StrToInterpolation(const Str: string): TInterpolation;
begin
  if not TryStrToInterpolation(Str, result) then
    raise Exception.CreateFmt(SErrInvalidInterpolationFmt, [Str]);
end;

class function TProcessor.TryNameToInterpolation(const Name :string; out Value :TInterpolation) :boolean;
var
  i :TInterpolation;
begin
  for i:=Low(TInterpolation) to High(TInterpolation) do
    if SameText(Name, INTERPOLATION_NAMES[i]) then begin
      Value := i;
      Exit(true);
    end;
  result := false;
end;

class function TProcessor.NameToInterpolation(const Name :string) :TInterpolation;
begin
  if not TryNameToInterpolation(Name, result) then
    raise Exception.CreateFmt(SErrInvalidInterpolationFmt, [Name]);
end;

procedure TProcessor.SetSizes(AValue :string);
begin
  if not TryStrToSizes(AValue, FSizes) then
    raise Exception.Create(Format(SErrInvalidSizesFmt, [AValue]));
end;

function TProcessor.GetSizes: string;
begin
  result := SizesToStr(FSizes);
end;

function TProcessor.GetSize: integer;
begin
  if Length(FSizes)=0 then
    result := 0
  else
    result := FSizes[0];
end;

function TProcessor.GetRename: boolean;
begin
  result := FTargetFilename<>'';
end;

function TProcessor.GetInterpolation: TInterpolation;
begin
  if FResampleMode = rmSimpleStretch then
    result := ipStretch
  else
    result := TInterpolation(integer(FResampleFilter)+2);
end;

function TProcessor.GetSourceFilenames: TStrings;
begin
  result := FSourceFilenames;
end;

procedure TProcessor.SetActive(AValue: boolean);
begin
  if AValue=Active then Exit;
  if Active then begin
    FCancelled := true;
  end else begin
    DoExecute;
  end;
end;

procedure TProcessor.SetSharpen(AValue: integer);
begin
  if FSharpen<0 then
    raise Exception.CreateFmt(SErrInvalidSharpenStrFmt, [AValue]);
  FSharpen:=AValue;
end;

procedure TProcessor.SetSize(AValue: integer);
begin
  if AValue<=0 then
    raise Exception.CreateFmt(SErrInvalidSizesFmt, [IntToStr(AValue)]);
end;

procedure TProcessor.SetInterpolation(AValue: TInterpolation);
begin
  case AValue of
  ipDefault:
    begin
      FResampleMode := rmFineResample;
      FResampleFilter := rfBestQuality;
    end;
  ipStretch:
    begin
      FResampleMode := rmSimpleStretch;
      FResampleFilter := rfLanczos2;
    end;
  else
    begin
      FResampleMode := rmFineResample;
      FResampleFilter := TResampleFilter(integer(AValue)-2);
    end;
  end;
end;

procedure TProcessor.SetJPEGQuality(AValue: integer);
begin
  if AValue<1 then AValue := 1;
  if AValue>100 then AValue := 100;
  if FJPEGQuality=AValue then Exit;
  FJPEGQuality:=AValue;
end;

procedure TProcessor.SetPNGCompression(AValue: integer);
begin
  if FPNGCompression=AValue then Exit;
  if (AValue<0) or (AValue>3) then
    raise Exception.CreateFmt(SErrInvalidPNGCompressionFmt, [AValue]);
  FPNGCompression:=AValue;
end;

procedure TProcessor.SetWatermarkParams(const AValue: TWatermarkParams);
begin
  FWatermarkParams := AValue;
  with FWatermarkParams do begin
    if Size<0.0 then Size := 1.0 else if Size>100.0 then Size := 100.0;
    if X<0.0 then X:=0.0 else if X>100.0 then X:=100.0;
    if Y<0.0 then Y:=0.0 else if Y>100.0 then Y:=100.0;
    if Opacity<0.0 then Opacity:=0.0 else if Opacity>100.0 then Opacity:=100.0;
  end;
end;

procedure TProcessor.SetSourceFilenames(AValue: TStrings);
begin
  FSourceFilenames.Assign(AValue);
end;

procedure TProcessor.SetTargetFolder(AValue: string);
begin
  AValue := IncludeTrailingPathDelimiter(AValue);
  if FTargetFolder=AValue then Exit;
  FTargetFolder:=AValue;
end;

class function TProcessor.GetVersion: string;
begin
  result := IMGRESVER;
end;

function TProcessor.Execute: boolean;
begin
  Active := true;
  result := FSuccess
end;

initialization
begin
  GetLocaleFormatSettings($409, TProcessor.FormatSettings);
  Randomize;
end;

end.

