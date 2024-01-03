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
//  The Processor can spawn a Task for each image to be processed.
//
//  It bases heavily on the BGRABitmap library and Atomeks threading.dispatcher
//  library.
//
////////////////////////////////////////////////////////////////////////////////

{$mode Delphi}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, StrUtils, Types, BGRABitmap, BGRABitmapTypes,
  threading.dispatcher, tags, logging;

type
  TTagsSource = (tsEXIF, tsTagsFiles); // Tags from EXIF and/or .tags files
  TTagsSources = set of TTagsSource;
  TScalingDirection = (sdUpScaling, sdNoscaling, sdDownScaling);

  TResampling = (rsStretch, rsBox, rsLinear, rsHalfCosine, rsCosine, rsBicubic,
    rsMitchell, rsSpline, rsLanczos2, rsLanczos3, rsLanczos4, rsBestQuality);

const
  RESAMPLING_STRINGS :array[TResampling] of string = (
    'Stretch', 'Box', 'Linear', 'Half Cosine', 'Cosine', 'Bicubic',
    'Mitchell', 'Spline', 'Lanczos 2', 'Lanczos 3', 'Lanczos 4', 'Best Quality');
  RESAMPLING_NAMES :array[TResampling] of string = (
    'Stretch', 'Box', 'Linear', 'HalfCosine', 'Cosine', 'Bicubic',
    'Mitchell', 'Spline', 'Lanczos2', 'Lanczos3', 'Lanczos4', 'BestQuality');

const
  IMGRESVER = '3.3';
  IMGRESCPR = 'imgres '+IMGRESVER+' © 2024 Jan Schirrmacher, www.atomek.de';

  TAGSREPORTFILETITLE       = '.tagsreport';
  IMAGEINFOSFILETITLE       = '.imageinfos';

  DEFAULTPNGCOMPRESSION     = 2;
  DEFAULTJPGQUALITY         = 75;
  DEFAULT_RESAMPLING        = rsLanczos2;
  DEFAULTMRKSIZE            = 20.0;
  DEFAULTMRKX               = 98.0;
  DEFAULTMRKY               = 98.0;
  DEFAULTMRKALPHA           = 50.0;
  DEFAULT_THREADCOUNT       = 0;
  DEFAULT_STOPONERROR       = true;
  DEFAULT_RENENABLED        = false;
  DEFAULT_RENFMTSTR         = 'img%2:s.%1:s';
  DEFAULT_RENFILETEMPLATE   = 'img%INDEX:1,3%.%FILEEXT%';
  DEFAULT_RENINDEXSTART     = 1;
  DEFAULT_RENINDEXDIGITS    = 3;
  DEFAULT_SHUFFLE           = false;
  DEFAULT_SHUFFLESEED       = 0;
  DEFAULT_FILETAGS          = nil;
  DEFAULT_COPYRIGHT         = '';
  DEFAULT_TAGSFMT           = '.images%d';
  DEFAULT_TAGSOURCES        :TTagsSources = [];
  DEFAULT_TAGSREPORT        = true;
  DEFAULT_NOCREATE          = false;

  DEFSIZES :array[0..15] of integer = (32, 48, 64, 120, 240, 360, 480, 640, 800, 960, 1280, 1600, 1920, 2560, 3840, 4096);

type

  TSizes = array of integer;

//  TPrintEvent = procedure(Sender :TObject; const Line :string) of object;
  TPrintEvent = procedure(Sender :TObject; const Line :string; Level :TLogLevel = llInfo) of object;
  TProgressEvent = procedure(Sender :TObject; Progress :single) of object;

  //////////////////////////////////////////////////////////////////////////////
  // Main class: resamples a list of images to a list of images and sizes

  { TProcessor }

  TProcessor = class
  public type

    TRenameParams = record
      Enabled :boolean;
      FmtStr :string;
      IndexDigits :integer;
      IndexStart :integer;
    end;

  private type

    // Needed while processing

    { TExecutionRessources }

    TExecutionRessources = class // Temporary process global ressources while processing
      IsMultipleTargetFolderStrategy :boolean;
      IsTargetFileRenamingStrategy :boolean;
      SourceFilenames :array of string; // After Shaking
      MrkImages :array of TBGRABitmap;  // Empty, One or for each Size
      TargetFolders :array of string;   // One folder for each Size
      TargetFoldersImageInfos :array of string;  // Names of the .imageinfos files, each folder has at least one
      FilesTags :TFilesTags;            // A database of all tags of all files
      TagsRequired :boolean;
      constructor Create;
      destructor Destroy; override;
    end;

    { TResampleTask }

    TResampleTask = class(TCustomTask) // Each image is a task
    protected
      function Execute(Context :TContext) :boolean; override;
      function GetTaskSteps :integer; override;
    public
      Processor :TProcessor;
      ProcRes :TExecutionRessources;
      SourceFilename :string;
      SourceFileIndex :integer;
      FileTags :TTags;
    end;

  private // Processing Params
    FSourceFilenames  :TStrings;
    FTargetFolder     :string;
    FSizes            :TSizes;
    FJpgQuality       :integer;
    FPngCompression   :integer;
    FResampleMode     :TResampleMode;
    FResampleFilter   :TResampleFilter;
    FMrkFilename      :string;
    FMrkFilenameDependsOnSize :boolean; // if MrkFilename contains %SIZE%
    FMrkSize          :single;
    FMrkX             :single;
    FMrkY             :single;
    FMrkAlpha         :single;
    FThreadCount      :integer;
    FStopOnError      :boolean;
    FRen              :TRenameParams;
    FShuffle          :boolean;
    FShuffleSeed      :integer;
    FTagsSources      :TTagsSources;
    FTagIDs           :TTagIDs; // 'Copyright',
    FCopyright        :string;
    FTagsReport       :boolean;
    FImageInfos       :boolean;
    FNoCreate         :boolean;
  private
    FCancel :boolean;
    FOnPrint :TPrintEvent;
    FOnProgress :TProgressEvent;
    function GetTargetFiletemplate: string;
    function GetResampling: TResampling;
    function GetSizes: string;
    function GetSourceFilenames: TStrings;
    procedure SetTargetFiletemplate(AValue: string);
    procedure SetResampling(AValue: TResampling);
    procedure SetMrkFilename(AValue: string);
    procedure SetSizes(AValue: string);
    procedure SetJpgQuality(AValue: integer);
    procedure SetPngCompression(AValue: integer);
    procedure SetMrkSize(AValue: single);
    procedure SetMrkX(AValue: single);
    procedure SetMrkY(AValue: single);
    procedure SetMrkAlpha(AValue: single);
    procedure SetSourceFilenames(AValue: TStrings);
    procedure SetTargetFolder(AValue: string);
    procedure SetThreadCount(AValue: integer);
    procedure SetShuffleSeed(AValue :integer);
    function ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
    class function CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
    procedure OnTaskPrint(Sender :TObject; WorkerId: integer; const Line :string; Level :TLevel);
    procedure OnTaskProgress(Sender :TObject; Progress :single);
    procedure Print(const Line :string; Level :TLevel = mlInfo);
  public
    constructor Create;
    destructor Destroy; override;
    class function GetVersion: string;
    function Execute :boolean;
    procedure Cancel;
    class function TryStrToPngCompression(const Str :string; out Value :integer) :boolean;
    class function PngCompressionToStr(const Value :integer) :string;
    class function TryStrToJpgQuality(const Str :string; out Value :integer) :boolean;
    class function JpgQualityToStr(const Value :integer) :string;
    class function TryStrToRenameParams(const Str :string; out Params :TRenameParams; out ErrStr :string) :boolean;
    class function RenameParamsToStr(const Params :TRenameParams) :string;
    class function TryStrToTagsSources(const Str :string; out Value :TTagsSources) :boolean;
    class function TryStrToResampling(const Str :string; out Value :TResampling) :boolean;
    class function StrToResampling(const Str :string) :TResampling;
    class function TryNameToResampling(const Str :string; out Value :TResampling) :boolean;
    class function NameToResampling(const Str :string) :TResampling;

    property SourceFilenames :TStrings read GetSourceFilenames write SetSourceFilenames;
    property TargetFolder :string read FTargetFolder write SetTargetFolder;
    property TargetFiletemplate :string read GetTargetFiletemplate write SetTargetFiletemplate;
    property Sizes :string read GetSizes write SetSizes;
    property JpgQuality :integer read FJpgQuality write SetJpgQuality;
    property PngCompression :integer read FPngCompression write SetPngCompression;
    property Resampling :TResampling read GetResampling write SetResampling;
    property MrkFilename :string read FMrkFilename write SetMrkFilename; // if msFile
    property MrkSize :single read FMrkSize write SetMrkSize;
    property MrkX :single read FMrkX write SetMrkX;
    property MrkY :single read FMrkY write SetMrkY;
    property MrkAlpha :single read FMrkAlpha write SetMrkAlpha;
    property ThreadCount :integer read FThreadCount write SetThreadCount;
    property StopOnError :boolean read FStopOnError write FStopOnError;
    property RenEnabled :boolean read FRen.Enabled;
    property Shuffle :boolean read FShuffle write FShuffle;
    property ShuffleSeed :integer read FShuffleSeed write SetShuffleSeed;
    property TagsSources :TTagsSources read FTagsSources write FTagsSources;
    property TagIDs :TStringArray read FTagIDs write FTagIDs;
    property Copyright :string read FCopyright write FCopyright;
    property TagsReport :boolean read FTagsReport write FTagsReport;
    property ImageInfos :boolean read FImageInfos write FImageInfos;
    property NoCreate :boolean read FNoCreate write FNoCreate;
    property OnPrint :TPrintEvent read FOnPrint write FOnPrint;
    property OnProgress :TProgressEvent read FOnProgress write FOnProgress;
  end;

function TrySizesStrToSizes(const Str :string; out Values :TSizes) :boolean;
function SizesToSizesStr(const Sizes :TSizes) :string;

implementation

uses
  Math, ZStream, FPWriteJpeg, FPWritePng, FPImage, utils,
  generics.collections, EXIFUtils;

const
  SCptPNGCompNone    = 'none';
  SCptPNGCompFastest = 'fastest';
  SCptPNGCompDefault = 'default';
  SCptPNGCompMax     = 'max';

  SCptJpgQualityDefault = 'default';

resourcestring
  SCptHint                        = 'Hint';
  SCptWarning                     = 'Warning';
  SCptAbort                       = 'Abort';
  SCptFatal                       = 'Fatal';
  SMsgWarningNoCreate             = 'Not really creating any image or folder (-nocreate flag)';
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
  SErrMultipleSizes               = 'Multiple sizes but placeholder %SIZE% not found in either folder or filename template';
  SErrInvalidPngCompressionFmt    = 'Invalid png compression %d (0..3 expected)';
  SErrInvalidJpgQualityFmt        = 'Invalid JPEG quality %d (1..100 expected)';
  SErrInvalidRenamingParamFmt     = 'Invalid renaming parameter ''%s''';
  SErrInvalidINDEXPlaceholderFmt  = 'Invalid INDEX placeholder parameters ''%s''';
  SErrInvalidINDEXStartFmt        = 'Invalid INDEX start ''%s''';
  SErrInvalidINDEXDigitsFmt       = 'Invalid INDEX digits number ''%s''';
  SErrInvalidINDEXParamCountFmt   = 'Invalid INDEX parameter count ''%s'' (2 expected)';
  SErrInvalidPlaceholder          = 'Unknown or invalid placeholder';
  SInfResultFmt                   = 'Images: %d, Filter: %s, Sizes: %d, Tasks: %d, Successful: %d, Failed: %d, Elapsed: %.2fs';
  SErrInvalidResamplingFmt        = 'Invalid resampling value ''%s''';
const
  PNGCOMPRS :array[0..3] of string = (SCptPNGCompNone, SCptPNGCompFastest, SCptPNGCompDefault, SCptPNGCompMax);
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

function TrySizesStrToSizes(const Str :string; out Values :TSizes) :boolean;
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

function SizesToSizesStr(const Sizes :TSizes) :string;
var
  i :integer;
begin
  if Length(Sizes)=0 then
    Exit('');
  result := IntToStr(Sizes[0]);
  for i:=1 to High(Sizes) do
    result := result + ', ' + IntToStr(Sizes[i]);
end;

function ExtractExt(const Filename :string) :string;
begin
  result := ExtractFileExt(Filename);
  if (Length(result)>0) then
    result := Copy(result, 2, Length(result)-1);
end;

function IsJPEG(const Filename :string) :boolean;
var
  Ext :string;
begin
  Ext := UpperCase(ExtractFileExt(Filename));
  result := (Ext = '.JPG') or (Ext = '.JPEG');
end;

function IsPNG(const Filename :string) :boolean;
begin
  result := UpperCase(ExtractFileExt(Filename)) = '.PNG'
end;

{ TProcessor.TExecutionRessources }

constructor TProcessor.TExecutionRessources.Create;
begin
  FilesTags := TFilesTags.Create;
end;

destructor TProcessor.TExecutionRessources.Destroy;
var
  i :integer;
begin
  FilesTags.Free;
  for i:=0 to High(MrkImages) do
    MrkImages[i].Free;
  inherited;
end;

{ TProcessor.TResampleTask }

function TProcessor.TResampleTask.Execute(Context: TContext): boolean;
var
  SourceImg :TBGRABitmap;
  TargetFolder :string;
  TargetFileExt :string;
  TargetFiletitle :string;
  TargetFiletitleExt :string;
  TargetFilename :string;
  TargetImg :TBGRABitmap;
  Writer :TFPCustomImageWriter;
  Size :integer;
  SourceSize :TSize;
  TargetSize :TSize;
  MrkImg :TBGRABitmap;
  MrkRectSize :TSize;
  MrkRect :TRect;
  IndexStr :string; // Index to display
  SizeStr :string;
  i, n, m :integer;
  ExifTags :TTags;
  ExifTagIds :TTagIDs;
  TagID :string;
  MsgScalingFmt :string;
  MsgScalingLevel :TLevel;
begin

  result := false;
  if Context.Cancelled then
    Exit;

  SourceImg := nil;

  // Source File
  try
    if Context.Cancelled then Exit;

    // Load source file...
    Print(Format(SMsgLoadingFmt, [SourceFilename]));
    SourceImg := TBGRABitmap.Create(SourceFilename);

    if Context.Cancelled then Exit;
    Progress(1);

    if (tsEXIF in Processor.FTagsSources) and IsJPEG(SourceFilename) then begin
      // Load EXIF
      Print(Format(SMsgReadingExifFmt, [SourceFilename]));
      ExifTags := TTags.Create;
      try
        ReadExifTags(SourceFilename, ExifTags, ExifTagIds);
        for TagId in ExifTagIds do
          if not FileTags.ContainsKey(TagId) then
            FileTags.AddOrSetValue(TagId, ExifTags[TagId]);
      finally
        ExifTags.Free;
      end;
    end;

    if Processor.FCopyright<>'' then
      FileTags.AddOrSetValue(TAGID_COPYRIGHT, Processor.FCopyright);

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

          if not Processor.FNoCreate then begin
            // Jpg-options
            Writer := TFPWriterJPEG.Create;
            with TFPWriterJPEG(Writer) do
              CompressionQuality := TFPJPEGCompressionQuality(Processor.JpgQuality);
          end;

        end else if IsPNG(SourceFilename) then begin

          if not Processor.FNoCreate then begin
            // Png-options
            Writer := TFPWriterPNG.Create;
            with TFPWriterPNG(Writer) do
              CompressionLevel := ZStream.TCompressionLevel(Processor.PngCompression);
          end;

        end else
          raise Exception.CreateFmt(SErrFormatNotSupportedFmt, [TargetFileExt]);

        // Calculate new size
        Size := Processor.FSizes[i];
        SourceSize := TSize.Create(SourceImg.Width, SourceImg.Height);
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

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        ////////////////////////////////////////////////////////////////////////////
        // Watermark
        if Processor.MrkFilename<>'' then begin
          MrkImg := ProcRes.MrkImages[i mod Length(ProcRes.MrkImages)];

          // Watermark size in percent of the width or original size if MrkSize=0.0
          if Processor.MrkSize<>0.0 then begin
            MrkRectSize.cx := round(TargetSize.cx*Processor.MrkSize/100.0);
            MrkRectSize.cy := round(TargetSize.cx*Processor.MrkSize/100.0 * MrkImg.Height/MrkImg.Width);
          end else begin
            MrkRectSize.cx := MrkImg.Width;
            MrkRectSize.cy := MrkImg.Height;
          end;
          MrkRect.Left := round((TargetSize.cx - MrkRectSize.cx) * Processor.MrkX/100.0);
          MrkRect.Top := round((TargetSize.cy - MrkRectSize.cy) * Processor.MrkY/100.0);
          MrkRect.Width := MrkRectSize.cx;
          MrkRect.Height := MrkRectSize.cy;
          Print(Format(SMsgWatermarkingFmt, [ExtractFilename(SourceFilename), SourceSize.cx, SourceSize.cy, TargetSize.cx, TargetSize.cy]));
          TargetImg.StretchPutImage(MrkRect, MrkImg, dmLinearBlend, round(255*Processor.MrkAlpha/100.0));
        end;

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        // Saving...
        if Processor.FRen.Enabled then begin
          // Specialize the prepared template
          // %FILENAME%
          TargetFiletitle := ExtractFilename(SourceFilename);
          if Length(TargetFileExt)>0 then
            TargetFiletitle := Copy(TargetFiletitle, 1, Length(TargetFiletitle)-Length(TargetFileExt)-1);
          // %FILEEXT% - has been extracted previously
          // %INDEX%
          if Processor.FRen.IndexDigits = 0 then
            IndexStr := IntToStr(SourceFileIndex)
          else begin
            if Processor.FRen.IndexDigits = -1 then
              n := round(log10(Processor.FSourceFilenames.Count))+1
            else
              n := Processor.FRen.IndexDigits;
            IndexStr := Format('%*.*d', [n, n, SourceFileIndex+Processor.FRen.IndexStart]);
          end;
          // %SIZE%
          SizeStr := IntToStr(Size);
          TargetFiletitleExt := Format(Processor.FRen.FmtStr,
            [TargetFiletitle, TargetFileExt, IndexStr, SizeStr, RESAMPLING_NAMES[Processor.Resampling]]);
        end else
          TargetFiletitleExt := ExtractFilename(SourceFilename);

        // Save
        TargetFolder := ProcRes.TargetFolders[i mod Length(ProcRes.TargetFolders)];
        TargetFilename := IncludeTrailingPathDelimiter(TargetFolder) + TargetFiletitleExt;
        Print(Format(SMsgSavingFmt, [TargetFilename]));
        if not Processor.FNoCreate then
          TargetImg.SaveToFile(TargetFilename, Writer);

        // Store TargetFilename in FileTags
        if ProcRes.TagsRequired then
          FileTags.Add(IntToStr(Size), TargetFilename);

      finally
        Writer.Free;
        TargetImg.Free;
      end;

      // EXIF
      if (Length(Processor.FTagIds)>0) and IsJPEG(TargetFilename) then begin
        Print(Format(SMsgWritingExifFmt, [TargetFilename]));
        if not Processor.FNoCreate then
          WriteExifTags(TargetFilename, FileTags, Processor.FTagIds);
      end;

      Progress(1);

    end;
    result := true;
  finally
    SourceImg.Free;
  end;
end;

function TProcessor.TResampleTask.GetTaskSteps: integer;
begin
  result := 1 + 3*Length(Processor.FSizes);
end;

{ TProcessor }

constructor TProcessor.Create;
begin
  FSourceFilenames     := TStringList.Create;
  FSizes            := nil;
  FJpgQuality       := DEFAULTJPGQUALITY;
  FPngCompression   := DEFAULTPNGCOMPRESSION;
  Resampling        := DEFAULT_RESAMPLING;
  FMrkFilename      := '';
  FMrkSize          := DEFAULTMRKSIZE;
  FMrkX             := DEFAULTMRKX;
  FMrkY             := DEFAULTMRKY;
  FMrkAlpha         := DEFAULTMRKALPHA;
  FThreadCount      := DEFAULT_THREADCOUNT;
  FStopOnError      := DEFAULT_STOPONERROR;
  FRen.Enabled      := DEFAULT_RENENABLED;
  FRen.FmtStr       := DEFAULT_RENFMTSTR;
  FRen.IndexStart   := DEFAULT_RENINDEXSTART;
  FRen.IndexDigits  := DEFAULT_RENINDEXDIGITS;
  FShuffle          := DEFAULT_SHUFFLE;
  FShuffleSeed      := DEFAULT_SHUFFLESEED;
  FTagsSources      := DEFAULT_TAGSOURCES;
  FTagIDs           := nil;
  FCopyright        := DEFAULT_COPYRIGHT;
  FTagsReport       := DEFAULT_TAGSREPORT;
  FNoCreate         := DEFAULT_NOCREATE;
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
  if Assigned(FOnPrint) then begin
    OnTaskPrint(self, -1, Line, Level);
  end;
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
    if FCancel then
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

function TProcessor.Execute :boolean;
var
  Exer :TExecutionRessources;
  Task :TResampleTask;
  Tasks :TTasks;
  Dispatcher :TDispatcher;
  i, j, n, m :integer;
  Item :string;
  TagsFilename :string;
  ImageInfosFilename :string;
begin
  // Check main parameters
  if FSourceFilenames.Count=0 then
    Exit(true);

  if Length(FSizes)=0 then
    raise Exception.Create(SErrMissingSizes);

  FCancel     := false;
  Exer        := TExecutionRessources.Create;
  Tasks       := TTasks.Create;
  Dispatcher  := TDispatcher.Create;
  try

    if FNoCreate then
      Print(SMsgWarningNoCreate, mlWarning);

    // Build list of SourceFilenames with absolute paths
    n := FSourceFilenames.Count;
    SetLength(Exer.SourceFilenames, n);
    for i:=0 to n-1 do
      Exer.SourceFilenames[i] := ExpandFilename(FSourceFilenames[i]);

    // If the files list has to be shuffled
    if FShuffle then begin
      Print(SMsgShakingFiles);
      if FShuffleSeed=0 then Randomize else RandSeed := FShuffleSeed;
      for i:=0 to n-1 do begin
        j := Random(n);
        Item := Exer.SourceFilenames[i];
        Exer.SourceFilenames[i] := Exer.SourceFilenames[j];
        Exer.SourceFilenames[j] := Item;
      end;
    end;

    // Check, if EXIF must be read
    if (FTagsSources=[]) and ((Length(FTagIds)>0) or FTagsReport or FImageInfos) then begin
      include(FTagsSources, tsEXIF);
      include(FTagsSources, tsTagsFiles);
    end;

    // A Copyright overwrite implicitely requires tagging
    if (FCopyright<>'') and not FTagIDs.contains(TAGID_COPYRIGHT) then
      FTagIDs.Add(TAGID_COPYRIGHT);

    // Check if the tags database is required
    Exer.TagsRequired := (FTagsSources<>[]) or (Length(FTagIds)>0);
    if Exer.TagsRequired then begin
      for i:=0 to n-1 do
        Exer.FilesTags.add(Exer.SourceFilenames[i]);

      // Load Tags
      if tsTagsFiles in FTagsSources then begin
        Print(SMsgLoadingTags);
        Exer.FilesTags.LoadTagsFiles;
      end;
    end;

    // Prepare the destination file renaming feature
    m := Length(FSizes);

    // Check, if multiple sizes, then either %SIZE% must be in folder or in renamed filename
    Exer.IsTargetFileRenamingStrategy := FRen.Enabled and (Pos('%3:', FRen.FmtStr)>0);
    if Length(FSizes)>1 then begin
      Exer.IsMultipleTargetFolderStrategy := Pos('%SIZE%', FTargetFolder)>0;
      if not Exer.IsMultipleTargetFolderStrategy and not Exer.IsTargetFileRenamingStrategy then
        raise Exception.Create(SErrMultipleSizes);
    end else
      Exer.IsMultipleTargetFolderStrategy := false;

    // Create Destination Folders
    SetLength(Exer.TargetFolders, m);
    if Exer.IsMultipleTargetFolderStrategy then
      for i:=0 to m-1 do
        Exer.TargetFolders[i] := IncludeTrailingPathDelimiter(ExpandFilename(ReplaceStr(FTargetFolder, '%SIZE%', IntToStr(FSizes[i]))))
    else
      for i:=0 to m-1 do
        Exer.TargetFolders[i] := IncludeTrailingPathDelimiter(ExpandFilename(FTargetFolder));
    for i:=0 to m-1 do begin
      Print(Format(SMsgCreatingFolderFmt, [Exer.TargetFolders[i]]));
      if not FNoCreate then
        ForceDirectories(Exer.TargetFolders[i]);
    end;

    // Delete .imageinfos
    // Depending on the Strategies IsMultipleTargetFolders and IsTargetFileRenamingStrategy
    // there a 3 tactics of storing .imageinfos
    for i:=0 to m-1 do begin
      if Exer.IsMultipleTargetFolderStrategy or not Exer.IsTargetFileRenamingStrategy then
        ImageInfosFilename := Exer.TargetFolders[i]+IMAGEINFOSFILETITLE
      else
        ImageInfosFilename := Exer.TargetFolders[i]+IMAGEINFOSFILETITLE+IntToStr(FSizes[i]);
      if FileExists(ImageInfosFilename) then begin
        Print(Format(SMsgDeletingImageInfosFmt, [ImageInfosFilename]));
        DeleteFile(ImageInfosFilename);
      end;
    end;

    // Load MrkImages...
    if FMrkFilename='' then begin
      SetLength(Exer.MrkImages, 0);
    end else begin
      if FMrkFilenameDependsOnSize then begin
        SetLength(Exer.MrkImages, m);
        for i:=0 to m-1 do begin
          Item := ReplaceStr(FMrkFilename, '%SIZE%', IntToStr(FSizes[i]));
          Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(Item)]));
          Exer.MrkImages[i] := TBGRABitmap.Create(Item);
        end;
      end else begin
        SetLength(Exer.MrkImages, 1);
        Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(FMrkFilename)]));
        Exer.MrkImages[0] := TBGRABitmap.Create(FMrkFilename);
      end;
    end;

    // For each SourceFilename create a task and prepare the tasks parameters
    Tasks.Capacity := n;
    for i:=0 to n-1 do begin
      Task := TResampleTask.Create;
      Task.Processor := self;
      Task.ProcRes := Exer;
      Task.SourceFilename := Exer.SourceFilenames[i];
      Task.SourceFileIndex := i;
      if Exer.TagsRequired then
        Exer.FilesTags.TryGetValue(Task.SourceFilename, Task.FileTags);
      Tasks.Add(Task);
    end;

    Dispatcher.OnPrint := OnTaskPrint;
    Dispatcher.OnProgress := OnTaskProgress;
    Dispatcher.MaxWorkerCount := ThreadCount;
    Dispatcher.StopOnError := StopOnError;

    result := Dispatcher.Execute(Tasks);

    if FTagsReport then begin
      if Exer.IsMultipleTargetFolderStrategy then
        TagsFilename := GetParentDirectory(FTargetFolder)+TAGSREPORTFILETITLE
      else
        TagsFilename := TargetFolder+TAGSREPORTFILETITLE;
      Print(Format(SMsgWritingTagsReportFmt, [TagsFilename]));
      Exer.FilesTags.SaveToFile(TagsFilename, Exer.FilesTags.TagIds, [soRelative]);
    end;

    if FImageInfos then begin
      for i:=0 to m-1 do begin
        if Exer.IsMultipleTargetFolderStrategy or not Exer.IsTargetFileRenamingStrategy then
          ImageInfosFilename := Exer.TargetFolders[i]+IMAGEINFOSFILETITLE
        else
          ImageInfosFilename := Exer.TargetFolders[i]+IMAGEINFOSFILETITLE+IntToStr(FSizes[i]);
        Print(Format(SMsgWritingTagsReportFmt, [ImageInfosFilename]));
        Exer.FilesTags.SaveImageInfos(ImageInfosFilename, FSizes[i]);
      end;
    end;

    if Assigned(FOnPrint) then with Dispatcher.Stats do
      FOnPrint(self, Format(SInfResultFmt, [n, RESAMPLING_STRINGS[Resampling], m, TaskCount, Successful, Failed, Elapsed/1000.0]), llNews);

  finally
    Dispatcher.Free;
    Tasks.Free;
    Exer.Free;
  end;

end;

procedure TProcessor.Cancel;
begin
  FCancel := true;
end;

class function TProcessor.TryStrToPngCompression(const Str: string; out Value: integer): boolean;
var
  i :integer;
begin
  for i:=0 to High(PNGCOMPRS) do
    if SameText(PNGCOMPRS[i], Str) then begin
        Value := i;
        Exit(true);
    end;
  result := false;
end;

class function TProcessor.PngCompressionToStr(const Value :integer) :string;
begin
  if (Value<0) or (Value>High(PNGCOMPRS)) then
    raise Exception.CreateFmt(SErrInvalidPngCompressionFmt, [Value]);
  result := PNGCOMPRS[Value];
end;

class function TProcessor.TryStrToJpgQuality(const Str :string; out Value :integer) :boolean;
begin
  if SameText(Trim(Str), SCptJpgQualityDefault) then begin
    Value := DEFAULTJPGQUALITY;
    result := true;
  end else
    result := TryStrToInt(Str, Value) and (Value>=1) and (Value<=100);
end;

class function TProcessor.JpgQualityToStr(const Value :integer) :string;
begin
  if (Value<1) or (Value>100) then
    raise Exception.CreateFmt(SErrInvalidJpgQualityFmt, [Value]);
  if Value=DEFAULTJPGQUALITY then
    result := SCptJpgQualityDefault
  else
    result := IntToStr(Value);
end;

class function TProcessor.TryStrToRenameParams(const Str: string; out Params: TRenameParams; out ErrStr: string): boolean;
var
  Placeholders :TStringArray;
  Items :TStringArray;
  i, ParamCount :integer;

  function IsPlaceHolder(const Placeholder :string; out Index :integer) :boolean;
  var
    i :integer;
  begin
    for i:=0 to High(Placeholders) do
      if SameText(Placeholder, Copy(Placeholders[i], 1, Length(Placeholder))) then begin
        Index := i;
        inc(ParamCount);
        Exit(true);
       end;
    result := false;
  end;

  function Err(const Msg :string) :boolean;
  begin
    ErrStr := Msg;
    result := false;
  end;

begin
  if not TryStrToPlaceholders(Str, '%', Placeholders) then
    Exit(Err(Format(SErrInvalidRenamingParamFmt, [Str])));

  Params.FmtStr := '';
  Params.Enabled := false;
  Params.IndexDigits := -1; // Default - as long as maximum Index needs + 1
  Params.IndexStart := 1;
  ParamCount := 0;
  if Length(Str)>0 then begin
    // %FILENAME%, %FILEEXT%, %INDEX:N,W%, %SIZE%
    // %0:s        %1:s       %2:s         %3:s
    // Default: img%2:s.%1:s
    Params.FmtStr := Str;
    if IsPlaceholder('FILEEXT', i) then
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%FILEEXT%', '%1:s');
    if IsPlaceholder('FILENAME', i) then
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%FILENAME%', '%0:s');
    if IsPlaceholder('INDEX', i) then begin
      // Parse Parameters
      if not TryParsePlaceholderParams(Placeholders[i], ':', Items) then
        Exit(Err(Format(SErrInvalidINDEXPlaceholderFmt, [Placeholders[i]])));
      if (Length(Items)>0) then begin
        if not (TryStrToInt(Items[0], Params.IndexStart)) or (Params.IndexStart<0) then
          Exit(Err(Format(SErrInvalidINDEXStartFmt, [Items[0]])));
        if (Length(Items)>1) then begin
          // auto, 0, 1, ...
          if SameText(Items[1], 'AUTO') then
            Params.IndexDigits := -1
          else if not (TryStrToInt(Items[1], Params.IndexDigits)) or (Params.IndexDigits<0) then
            Exit(Err(Format(SErrInvalidINDEXDigitsFmt, [Items[1]])));
        end else if (Length(Items)>2) then
          Exit(Err(Format(SErrInvalidINDEXParamCountFmt, [Placeholders[i]])));
      end;
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%'+Placeholders[i]+'%', '%2:s');
    end;
    if IsPlaceholder('SIZE', i) then
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%SIZE%', '%3:s');
    if IsPlaceholder('FILTER', i) then
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%FILTER%', '%4:s');
    if ParamCount<Length(Placeholders) then
      Exit(Err(SErrInvalidPlaceholder));
    Params.Enabled := true;
  end;
  result := true;
end;

class function TProcessor.RenameParamsToStr(const Params: TRenameParams): string;
begin
  if Params.Enabled then begin
    result := Format(Params.FmtStr, [
      '%FILENAME%', // 0
      '%FILEEXT%',  // 1
      '%'+Format('INDEX:%d,%d', [Params.IndexStart, Params.IndexDigits])+'%', // 2
      '%SIZE%',     // 3
      '%FILTER%'    // 4
    ]);
  end else
    result := '';
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

class function TProcessor.TryStrToResampling(const Str: string; out Value: TResampling): boolean;
var
  i :TResampling;
begin
  for i:=Low(TResampling) to High(TResampling) do
    if SameText(Str, RESAMPLING_STRINGS[i]) then begin
      Value := i;
      Exit(true);
    end;
  result := false;
end;

class function TProcessor.StrToResampling(const Str: string): TResampling;
begin
  if not TryStrToResampling(Str, result) then
    raise Exception.CreateFmt(SErrInvalidResamplingFmt, [Str]);
end;

class function TProcessor.TryNameToResampling(const Str :string; out Value :TResampling) :boolean;
var
  i :TResampling;
begin
  for i:=Low(TResampling) to High(TResampling) do
    if SameText(Str, RESAMPLING_NAMES[i]) then begin
      Value := i;
      Exit(true);
    end;
  result := false;
end;

class function TProcessor.NameToResampling(const Str :string) :TResampling;
begin
  if not TryNameToResampling(Str, result) then
    raise Exception.CreateFmt(SErrInvalidResamplingFmt, [Str]);
end;

procedure TProcessor.SetSizes(AValue :string);
begin
  if not TrySizesStrToSizes(AValue, FSizes) then
    raise Exception.Create(Format(SErrInvalidSizesFmt, [AValue]));
end;

function TProcessor.GetSizes: string;
begin
  result := SizesToSizesStr(FSizes);
end;

function TProcessor.GetTargetFiletemplate: string;
begin
  result := RenameParamsToStr(FRen);
end;

function TProcessor.GetResampling: TResampling;
begin
  if FResampleMode = rmSimpleStretch then
    result := rsStretch
  else
    result := TResampling(integer(FResampleFilter)+1);
end;

function TProcessor.GetSourceFilenames: TStrings;
begin
  result := FSourceFilenames;
end;

procedure TProcessor.SetTargetFiletemplate(AValue: string);
var
  ErrMsg :string;
  Params :TRenameParams;
begin
  if not TryStrToRenameParams(AValue, Params, ErrMsg) then
    raise Exception.Create(ErrMsg);
  FRen := Params;
end;

procedure TProcessor.SetResampling(AValue: TResampling);
begin
  if AValue=rsStretch then begin
    FResampleMode := rmSimpleStretch;
    FResampleFilter := rfLanczos2;
  end else begin
    FResampleMode := rmFineResample;
    FResampleFilter := TResampleFilter(integer(AValue)-1);
  end;
end;

procedure TProcessor.SetMrkFilename(AValue: string);
begin
  if AValue=FMrkFilename then Exit;
  FMrkFilename := AValue;
  FMrkFilenameDependsOnSize := Pos('%SIZE%', AValue)>0;
end;

procedure TProcessor.SetJpgQuality(AValue: integer);
begin
  if FJpgQuality=AValue then Exit;
  if (AValue<1) or (AValue>100) then
    raise Exception.CreateFmt(SErrInvalidJpgQualityFmt, [AValue]);
  FJpgQuality:=AValue;
end;

procedure TProcessor.SetPngCompression(AValue: integer);
begin
  if FPngCompression=AValue then Exit;
  if (AValue<0) or (AValue>3) then
    raise Exception.CreateFmt(SErrInvalidPNGCompressionFmt, [AValue]);
  FPngCompression:=AValue;
end;

procedure TProcessor.SetMrkSize(AValue: single);
begin
  if FMrkSize=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FMrkSize:=AValue;
end;

procedure TProcessor.SetMrkX(AValue: single);
begin
  if FMrkX=AValue then Exit;
  FMrkX:=AValue;
end;

procedure TProcessor.SetMrkY(AValue: single);
begin
  if FMrkY=AValue then Exit;
  FMrkY:=AValue;
end;

procedure TProcessor.SetMrkAlpha(AValue: single);
begin
  if FMrkAlpha=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FMrkAlpha:=AValue;
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

initialization
begin
  Randomize;
end;

end.

