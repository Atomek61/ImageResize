unit imgres;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2023 Jan Schirrmacher, www.atomek.de
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
  threading.dispatcher, tags;

type
  TTagsSource = (tsEXIF, tsTagsFiles); // Tags from EXIF and/or .tags files
  TTagsSources = set of TTagsSource;

const
  IMGRESVER = '3.2';
  IMGRESCPR = 'imgres '+IMGRESVER+' © 2023 Jan Schirrmacher, www.atomek.de';

  DEFAULTPNGCOMPRESSION    = 2;
  DEFAULTJPGQUALITY        = 75;
  DEFAULT_FILTER           = rfLanczos2;
  DEFAULTMRKSIZE           = 20.0;
  DEFAULTMRKX              = 98.0;
  DEFAULTMRKY              = 98.0;
  DEFAULTMRKALPHA          = 50.0;
  DEFAULT_THREADCOUNT      = 0;
  DEFAULT_STOPONERROR      = true;
  DEFAULT_RENENABLED       = false;
  DEFAULT_RENFMTSTR        = 'img%2:s.%1:s';
  DEFAULT_RENFILETEMPLATE  = 'img%INDEX:1,3%.%FILEEXT%';
  DEFAULT_RENINDEXSTART    = 1;
  DEFAULT_RENINDEXDIGITS   = 3;
  DEFAULT_SHAKE            = false;
  DEFAULT_SHAKESEED        = 0;
  DEFAULT_FILETAGS         = nil;
  DEFAULT_COPYRIGHT        = '';
  DEFAULT_TAGSREPORT       = 'imagelist.trp';
  DEFAULT_TAGSOURCES       :TTagsSources = [];

  DEFSIZES :array[0..15] of integer = (32, 48, 64, 120, 240, 360, 480, 640, 800, 960, 1280, 1600, 1920, 2560, 3840, 4096);

type

  TSizes = array of integer;

  TPrintEvent = procedure(Sender :TObject; const Line :string) of object;
  TProgressEvent = procedure(Sender :TObject; Progress :single) of object;

  //////////////////////////////////////////////////////////////////////////////
  // Main class: resamples a list a images to a list of sizes

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
      IsMultipleDstFolderStrategy :boolean;
      IsDstFileRenamingStrategy :boolean;
      SrcFilenames :array of string;    // After Shaking
      MrkImages :array of TBGRABitmap;  // Empty, One or for each Size
      DstFolders :array of string;      // One or for each Size
      FilesTags :TFilesTags;            // A database of all tags of all files
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
      SrcFilename :string;
      SrcFileIndex :integer;
      Tags :TTags;
    end;

  private // Processing Params
    FSrcFilenames    :TStrings;
    FDstFolder       :string;
    FSizes           :TSizes;
    FJpgQuality      :integer;
    FPngCompression  :integer;
    FFilter          :TResampleFilter;
    FMrkFilename     :string;
    FMrkFilenameDependsOnSize :boolean; // if MrkFilename contains %SIZE%
    FMrkSize         :single;
    FMrkX            :single;
    FMrkY            :single;
    FMrkAlpha        :single;
    FThreadCount     :integer;
    FStopOnError     :boolean;
    FRen             :TRenameParams;
    FShake           :boolean;
    FShakeSeed       :integer;
    FTagsSources     :TTagsSources;
    FTagIDs          :TTagIDs; // 'Copyright',
    FCopyright       :string;
    FTagsReportFilename :string;
  private
    FCancel :boolean;
    FOnPrint :TPrintEvent;
    FOnProgress :TProgressEvent;
    function GetDstFiletemplate: string;
    function GetFilter: string;
    function GetSizes: string;
    function GetSrcFilenames: TStrings;
    procedure SetDstFiletemplate(AValue: string);
    procedure SetFilter(AValue: string);
    procedure SetMrkFilename(AValue: string);
    procedure SetSizes(AValue: string);
    procedure SetJpgQuality(AValue: integer);
    procedure SetPngCompression(AValue: integer);
    procedure SetMrkSize(AValue: single);
    procedure SetMrkX(AValue: single);
    procedure SetMrkY(AValue: single);
    procedure SetMrkAlpha(AValue: single);
    procedure SetSrcFilenames(AValue: TStrings);
    procedure SetThreadCount(AValue: integer);
    procedure SetShakeSeed(AValue :integer);
    function ResampleImg(Img :TBgraBitmap; const Size :TSize; const Filter :TResampleFilter) :TBgraBitmap;
    class function CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
    procedure OnTaskPrint(Sender :TObject; WorkerId: integer; const Line :string; Level :TLevel);
    procedure OnTaskProgress(Sender :TObject; Progress :single);
    procedure Print(const Line :string; Level :TLevel = mlNormal);
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

    property SrcFilenames :TStrings read GetSrcFilenames write SetSrcFilenames;
    property DstFolder :string read FDstFolder write FDstFolder;
    property DstFiletemplate :string read GetDstFiletemplate write SetDstFiletemplate;
    property Sizes :string read GetSizes write SetSizes;
    property JpgQuality :integer read FJpgQuality write SetJpgQuality;
    property PngCompression :integer read FPngCompression write SetPngCompression;
    property Filter :string read GetFilter write SetFilter;
    property MrkFilename :string read FMrkFilename write SetMrkFilename; // if msFile
    property MrkSize :single read FMrkSize write SetMrkSize;
    property MrkX :single read FMrkX write SetMrkX;
    property MrkY :single read FMrkY write SetMrkY;
    property MrkAlpha :single read FMrkAlpha write SetMrkAlpha;
    property ThreadCount :integer read FThreadCount write SetThreadCount;
    property StopOnError :boolean read FStopOnError write FStopOnError;
    property RenEnabled :boolean read FRen.Enabled;
    property Shake :boolean read FShake write FShake;
    property ShakeSeed :integer read FShakeSeed write SetShakeSeed;
    property TagsSources :TTagsSources read FTagsSources write FTagsSources;
    property TagIDs :TStringArray read FTagIDs write FTagIDs;
    property Copyright :string read FCopyright write FCopyright;
    property TagsReportFilename :string read FTagsReportFilename write FTagsReportFilename;
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
  SCptHint    = 'Hint';
  SCptWarning = 'Warning';
  SCptAbort   = 'Abort';
  SCptFatal   = 'Fatal';
  SErrFormatNotSupportedFmt = 'Format %s not supported.';
  SWrnUpsamplingFmt = 'Upsampling ''%s''';
  SMsgResamplingFmt = 'Resampling ''%s'' from %dx%d to %dx%d...';
  SMsgWatermarkingFmt = 'Watermarking ''%s''...';
  SMsgSavingFmt = 'Saving ''%s''...';
  SMsgLoadingFmt = 'Loading ''%s''...';
  SMsgLoadingTags = 'Loading .tags...';
  SMsgWritingTagsReportFmt = 'Writing Tags Report to ''%s''...';
  SMsgReadingExifFmt = 'Reading EXIF from ''%s''...';
  SMsgCreatingFolderFmt = 'Creating folder ''%s''...';
  SMsgShakingFiles = 'Shaking list of files...';
  SMsgLoadMrkFileFmt = 'Loading Watermark ''%s''...';
  SMsgWritingExifFmt = 'Writing EXIF to ''%s''...';
  SErrInvalidThreadCount = 'Invalid threadcount.';
  SErrMissingSizes = 'Missing sizes.';
  SErrInvalidSizesFmt = 'Invalid sizes ''%s''.';
  SErrMultipleSizes = 'Multiple sizes but placeholder %SIZE% not found in either folder or filename template.';
  SErrInvalidPngCompressionFmt = 'Invalid png compression %d (0..3 expected).';
  SErrInvalidJpgQualityFmt = 'Invalid JPEG quality %d (1..100 expected).';
  SErrInvalidRenamingParamFmt = 'Invalid renaming parameter ''%s''.';
  SErrInvalidINDEXPlaceholderFmt = 'Invalid INDEX placeholder parameters ''%s''.';
  SErrInvalidINDEXStartFmt = 'Invalid INDEX start ''%s''.';
  SErrInvalidINDEXDigitsFmt = 'Invalid INDEX digits number ''%s''.';
  SErrInvalidINDEXParamCountFmt = 'Invalid INDEX parameter count ''%s'' (2 expected).';
  SErrInvalidPlaceholder = 'Unknown or invalid placeholder.';
  SInfResultFmt = 'Images: %d, Filter: %s, Sizes: %d, Tasks: %d, Successful: %d, Failed: %d, Elapsed: %.2fs';
  SErrInvalifFilterFmt = 'Invalid filter ''%s''.';

const
  PNGCOMPRS :array[0..3] of string = (SCptPNGCompNone, SCptPNGCompFastest, SCptPNGCompDefault, SCptPNGCompMax);
  LEVELSTRS :array[TLevel] of string = (SCptHint, '', SCptWarning, SCptAbort, SCptFatal);

type
  TIntegerArrayHelper = TArrayHelper<Integer>;

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
  SrcImg :TBGRABitmap;
  DstFolder :string;
  DstFileExt :string;
  DstFiletitle :string;
  DstFiletitleExt :string;
  DstFilename :string;
  DstImg :TBGRABitmap;
  Writer :TFPCustomImageWriter;
  Size :integer;
  SrcSize :TSize;
  DstSize :TSize;
  MrkImg :TBGRABitmap;
  MrkRectSize :TSize;
  MrkRect :TRect;
  IndexStr :string; // Index to display
  SizeStr :string;
  i, n, m :integer;
  ExifTags :TTags;
  ExifTagIds :TTagIDs;
  TagID :string;
begin

  result := false;
  if Context.Cancelled then
    Exit;

  SrcImg := nil;

  // Source File
  try
    if Context.Cancelled then Exit;

    // Load source file...
    Print(Format(SMsgLoadingFmt, [SrcFilename]));
    SrcImg := TBGRABitmap.Create(SrcFilename);

    if Context.Cancelled then Exit;
    Progress(1);

    if (tsEXIF in Processor.FTagsSources) and IsJPEG(SrcFilename) then begin
      // Load EXIF
      Print(Format(SMsgReadingExifFmt, [SrcFilename]));
      ExifTags := TTags.Create;
      try
        ReadExifTags(SrcFilename, ExifTags, ExifTagIds);
        for TagId in ExifTagIds do
          if not Tags.ContainsKey(TagId) then
            Tags.AddOrSetValue(TagId, ExifTags[TagId]);
      finally
        ExifTags.Free;
      end;
    end;

    if Processor.FCopyright<>'' then
      Tags.AddOrSetValue(TAGID_COPYRIGHT, Processor.FCopyright);

    m := Length(Processor.FSizes);
    for i:=0 to m-1 do begin

      Writer := nil;
      DstImg := nil;

      try
        // Create Writer depending on file extension
        DstFileExt := ExtractExt(SrcFilename);
        if IsJPEG(SrcFilename) then begin

          // Jpg-options
          Writer := TFPWriterJPEG.Create;
          with TFPWriterJPEG(Writer) do
            CompressionQuality := TFPJPEGCompressionQuality(Processor.JpgQuality);

        end else if IsPNG(SrcFilename) then begin

          // Png-options
          Writer := TFPWriterPNG.Create;
          with TFPWriterPNG(Writer) do
            CompressionLevel := ZStream.TCompressionLevel(Processor.PngCompression);

        end else
          raise Exception.CreateFmt(SErrFormatNotSupportedFmt, [DstFileExt]);

        // Calculate new size
        Size := Processor.FSizes[i];
        SrcSize := TSize.Create(SrcImg.Width, SrcImg.Height);
        DstSize := CalcResamplingSize(SrcSize, Size);

        // Warning, if upsampling
        if (SrcSize.cx<DstSize.cx) or (DstSize.cy<DstSize.cy) then
          Print(Format(SWrnUpsamplingFmt, [
             ExtractFilename(SrcFilename)]), mlWarning);

        ////////////////////////////////////////////////////////////////////////////
        // Resampling...
        Print(Format(SMsgResamplingFmt, [
          ExtractFilename(SrcFilename), SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));
        DstImg := Processor.ResampleImg(SrcImg, DstSize, Processor.FFilter);

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        ////////////////////////////////////////////////////////////////////////////
        // Watermark
        if Processor.MrkFilename<>'' then begin
          MrkImg := ProcRes.MrkImages[i mod Length(ProcRes.MrkImages)];

          // Watermark size in percent of the width or original size if MrkSize=0.0
          if Processor.MrkSize<>0.0 then begin
            MrkRectSize.cx := round(DstSize.cx*Processor.MrkSize/100.0);
            MrkRectSize.cy := round(DstSize.cx*Processor.MrkSize/100.0 * MrkImg.Height/MrkImg.Width);
          end else begin
            MrkRectSize.cx := MrkImg.Width;
            MrkRectSize.cy := MrkImg.Height;
          end;
          MrkRect.Left := round((DstSize.cx - MrkRectSize.cx) * Processor.MrkX/100.0);
          MrkRect.Top := round((DstSize.cy - MrkRectSize.cy) * Processor.MrkY/100.0);
          MrkRect.Width := MrkRectSize.cx;
          MrkRect.Height := MrkRectSize.cy;
          Print(Format(SMsgWatermarkingFmt, [ExtractFilename(SrcFilename), SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));
          DstImg.StretchPutImage(MrkRect, MrkImg, dmLinearBlend, round(255*Processor.MrkAlpha/100.0));
        end;

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        // Saving...
        if Processor.FRen.Enabled then begin
          // Specialize the prepared template
          // %FILENAME%
          DstFiletitle := ExtractFilename(SrcFilename);
          if Length(DstFileExt)>0 then
            DstFiletitle := Copy(DstFiletitle, 1, Length(DstFiletitle)-Length(DstFileExt)-1);
          // %FILEEXT% - has been extracted previously
          // %INDEX%
          if Processor.FRen.IndexDigits = 0 then
            IndexStr := IntToStr(SrcFileIndex)
          else begin
            if Processor.FRen.IndexDigits = -1 then
              n := round(log10(Processor.FSrcFilenames.Count))+1
            else
              n := Processor.FRen.IndexDigits;
            IndexStr := Format('%*.*d', [n, n, SrcFileIndex+Processor.FRen.IndexStart]);
          end;
          // %SIZE%
          SizeStr := IntToStr(Size);
          DstFiletitleExt := Format(Processor.FRen.FmtStr,
            [DstFiletitle, DstFileExt, IndexStr, SizeStr]);
        end else
          DstFiletitleExt := ExtractFilename(SrcFilename);

        // Save
        DstFolder := ProcRes.DstFolders[i mod Length(ProcRes.DstFolders)];
        DstFilename := IncludeTrailingPathDelimiter(DstFolder) + DstFiletitleExt;
        Print(Format(SMsgSavingFmt, [DstFilename]));
        DstImg.SaveToFile(DstFilename, Writer);

      finally
        Writer.Free;
        DstImg.Free;
      end;

      // EXIF
      if (Length(Processor.FTagIds)>0) and IsJPEG(DstFilename) then begin
        Print(Format(SMsgWritingExifFmt, [DstFilename]));
        WriteExifTags(DstFilename, Tags, Processor.FTagIds);
      end;

      Progress(1);

    end;
    result := true;
  finally
    SrcImg.Free;
  end;
end;

function TProcessor.TResampleTask.GetTaskSteps: integer;
begin
  result := 1 + 3*Length(Processor.FSizes);
end;

{ TProcessor }

constructor TProcessor.Create;
begin
  FSrcFilenames     := TStringList.Create;
  FSizes            := nil;
  FJpgQuality       := DEFAULTJPGQUALITY;
  FPngCompression   := DEFAULTPNGCOMPRESSION;
  FFilter           := DEFAULT_FILTER;
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
  FShake            := DEFAULT_SHAKE;
  FShakeSeed        := DEFAULT_SHAKESEED;
  FTagsSources      := DEFAULT_TAGSOURCES;
  FTagIDs           := nil;
  FCopyright        := DEFAULT_COPYRIGHT;
  FTagsReportFilename := '';
end;

destructor TProcessor.Destroy;
begin
  FSrcFilenames.Free;
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
begin
  if Assigned(FOnPrint) then begin
    Prefix := LEVELSTRS[Level];
    if Prefix<>'' then
      Prefix := Prefix + '  ';
    FOnPrint(self, Format('[%d] %s%s', [WorkerId+1, Prefix, Line]));
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

procedure TProcessor.SetShakeSeed(AValue: integer);
begin
  if AValue<0 then AValue := 0;
  FShakeSeed := AValue;
end;

function TProcessor.ResampleImg(Img :TBgraBitmap; const Size :TSize; const Filter :TResampleFilter) :TBgraBitmap;
begin
  Img.ResampleFilter := Filter;
  result := Img.Resample(Size.cx, Size.cy) as TBGRABitmap;
end;

function TProcessor.Execute :boolean;
var
  exer :TExecutionRessources;
  Task :TResampleTask;
  Tasks :TTasks;
  Dispatcher :TDispatcher;
  i, j, n, m :integer;
  x :string;
begin
  // Check main parameters
  if FSrcFilenames.Count=0 then
    Exit(true);

  if Length(FSizes)=0 then
    raise Exception.Create(SErrMissingSizes);

  FCancel     := false;
  exer        := TExecutionRessources.Create;
  Tasks       := TTasks.Create;
  Dispatcher  := TDispatcher.Create;
  try

    n := FSrcFilenames.Count;
    SetLength(exer.SrcFilenames, n);
    for i:=0 to n-1 do
      exer.SrcFilenames[i] := ExpandFilename(FSrcFilenames[i]);

    // If the files list has to be shaked
    if FShake then begin
      Print(SMsgShakingFiles);
      if FShakeSeed=0 then Randomize else RandSeed := FShakeSeed;
      for i:=0 to n-1 do begin
        j := Random(n);
        x := exer.SrcFilenames[i];
        exer.SrcFilenames[i] := exer.SrcFilenames[j];
        exer.SrcFilenames[j] := x;
      end;
    end;

    // Check, if the TagsSources are consistent with the other options
    if (FTagsSources=[]) and ((Length(FTagIds)>0) or (FTagsReportFilename<>'')) then
      include(FTagsSources, tsEXIF);

    // A Copyright overwrite implicitely requires tagging
    if (FCopyright<>'') and not FTagIDs.contains(TAGID_COPYRIGHT) then
      FTagIDs.Add(TAGID_COPYRIGHT);

    // If required, prepare the tags database
    if (FTagsSources<>[]) or (Length(FTagIds)>0) then begin
      for i:=0 to n-1 do
        exer.FilesTags.add(exer.SrcFilenames[i]);

      // Load Tags
      if tsTagsFiles in FTagsSources then begin
        Print(SMsgLoadingTags);
        exer.FilesTags.LoadTagsFiles;
      end;
    end;

    // Prepare the destination file renaming feature
    m := Length(FSizes);

    // Check, if multiple sizes, then either %SIZE% must be in folder or in renamed filename
    exer.IsDstFileRenamingStrategy := FRen.Enabled and (Pos('%3:s', FRen.FmtStr)>0);
    if Length(FSizes)>1 then begin
      exer.IsMultipleDstFolderStrategy := Pos('%SIZE%', FDstFolder)>0;
      if not exer.IsMultipleDstFolderStrategy and not exer.IsDstFileRenamingStrategy then
        raise Exception.Create(SErrMultipleSizes);
    end else
      exer.IsMultipleDstFolderStrategy := false;

    // Load MrkImages...
    if FMrkFilename='' then begin
      SetLength(exer.MrkImages, 0);
    end else begin
      if FMrkFilenameDependsOnSize then begin
        SetLength(exer.MrkImages, m);
        for i:=0 to m-1 do begin
          x := ReplaceStr(FMrkFilename, '%SIZE%', IntToStr(FSizes[i]));
          Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(x)]));
          exer.MrkImages[i] := TBGRABitmap.Create(x);
        end;
      end else begin
        SetLength(exer.MrkImages, 1);
        Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(FMrkFilename)]));
        exer.MrkImages[0] := TBGRABitmap.Create(FMrkFilename);
      end;
    end;

    // Create Destination Folders...
    SetLength(exer.DstFolders, m);
    for i:=0 to m-1 do begin
      x := ExpandFilename(ReplaceStr(FDstFolder, '%SIZE%', IntToStr(FSizes[i])));
      exer.DstFolders[i] := x;
    end;
    for i:=0 to High(exer.DstFolders) do begin
      Print(Format(SMsgCreatingFolderFmt, [exer.DstFolders[i]]));
      ForceDirectories(exer.DstFolders[i]);
    end;

    // For each SrcFilename create a task and prepare the tasks parameters
    Tasks.Capacity := n;
    for i:=0 to n-1 do begin
      Task := TResampleTask.Create;
      Task.Processor := self;
      Task.ProcRes := exer;
      Task.SrcFilename := exer.SrcFilenames[i];
      Task.SrcFileIndex := i;
      if Assigned(exer.FilesTags) then
        exer.FilesTags.TryGetValue(Task.SrcFilename, Task.Tags);
      Tasks.Add(Task);
    end;

    Dispatcher.OnPrint := OnTaskPrint;
    Dispatcher.OnProgress := OnTaskProgress;
    Dispatcher.MaxWorkerCount := ThreadCount;
    Dispatcher.StopOnError := StopOnError;

    result := Dispatcher.Execute(Tasks);

    if FTagsReportFilename<>'' then begin
      Print(Format(SMsgWritingTagsReportFmt, [FTagsReportFilename]));
      exer.FilesTags.SaveToFile(FTagsReportFilename, exer.FilesTags.TagIds, [soRelative]);
    end;

    if Assigned(FOnPrint) then with Dispatcher.Stats do
      FOnPrint(self, Format(SInfResultFmt, [n, Filter, m, TaskCount, Successful, Failed, Elapsed/1000.0]));

  finally
    Dispatcher.Free;
    Tasks.Free;
    exer.Free;
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
    if IsPlaceholder('FILEEXT', i) then begin
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%FILEEXT%', '%1:s');
    end;
    if IsPlaceholder('FILENAME', i) then begin
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%FILENAME%', '%0:s');
    end;
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
    if IsPlaceholder('SIZE', i) then begin
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%SIZE%', '%3:s');
    end;
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
      '%'+Format('INDEX:%d,%d', [Params.IndexStart, Params.IndexDigits])+'%',
      '%SIZE%'
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

procedure TProcessor.SetSizes(AValue :string);
begin
  if not TrySizesStrToSizes(AValue, FSizes) then
    raise Exception.Create(Format(SErrInvalidSizesFmt, [AValue]));
end;

function TProcessor.GetSizes: string;
begin
  result := SizesToSizesStr(FSizes);
end;

function TProcessor.GetDstFiletemplate: string;
begin
  result := RenameParamsToStr(FRen);
end;

function TProcessor.GetFilter: string;
begin
  result := ResampleFilterStr[FFilter];
end;

function TProcessor.GetSrcFilenames: TStrings;
begin
  result := FSrcFilenames;
end;

procedure TProcessor.SetDstFiletemplate(AValue: string);
var
  ErrMsg :string;
  Params :TRenameParams;
begin
  if not TryStrToRenameParams(AValue, Params, ErrMsg) then
    raise Exception.Create(ErrMsg);
  FRen := Params;
end;

procedure TProcessor.SetFilter(AValue: string);
var
  f :TResampleFilter;
begin
  for f := low(TResampleFilter) to high(TResampleFilter) do
    if CompareText(AValue, ResampleFilterStr[f])=0 then begin
      FFilter := f;
      Exit;
    end;
  raise Exception.CreateFmt(SErrInvalifFilterFmt, [AValue]);
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

procedure TProcessor.SetSrcFilenames(AValue: TStrings);
begin
  FSrcFilenames.Assign(AValue);
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

