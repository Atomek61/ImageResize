unit imgres;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2020 Jan Schirrmacher, www.atomek.de
//
//  See https://github.com/Atomek61/ImageResize.git for licensing
//
//  imgres.pas is the processing core of both, the CLI and the GUI applications.
//
//  It bases heavily on the BGRABitmap library and Atomeks threading.dispatcher
//  library.
//
////////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  Classes, SysUtils, StrUtils, Types, BGRABitmap, BGRABitmapTypes,
  threading.dispatcher, exifutils;

const
  IMGRESVER = '3.1';
  IMGRESCPR = 'imgres '+IMGRESVER+' © 2023 Jan Schirrmacher, www.atomek.de';

  DEFAULTPNGCOMPRESSION    = 2;
  DEFAULTJPGQUALITY        = 75;
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
  DEFAULT_TAGS             = [];
  DEFAULT_COPYRIGHT        = '';

  DEFSIZES :array[0..15] of integer = (32, 48, 64, 120, 240, 360, 480, 640, 800, 960, 1280, 1600, 1920, 2560, 3840, 4096);

type

  TSizes = array of integer;

  TPrintEvent = procedure(Sender :TObject; const Line :string) of object;
  TProgressEvent = procedure(Sender :TObject; Progress :single) of object;

  //////////////////////////////////////////////////////////////////////////////
  // Main class: resamples a list a images to a list of sizes

  { TImgRes }

  TImgRes = class
  public type

    TRenameParams = record
      Enabled :boolean;
      FmtStr :string;
      IndexDigits :integer;
      IndexStart :integer;
    end;

    TParams = record
      SrcFilenames :TStringList;
      DstFolder :string;
      Sizes :TSizes;
      JpgQuality :integer;
      PngCompression :integer;
      MrkFilename :string;
      MrkFilenameDependsOnSize :boolean; // if MrkFilename contains %SIZE%
      MrkSize :single;
      MrkX :single;
      MrkY :single;
      MrkAlpha :single;
      ThreadCount :integer;
      StopOnError :boolean;
      Ren :TRenameParams;
      Shake :boolean;
      ShakeSeed :integer;
      Tags :TTags;
      Copyright :string;
    end;

  private type

    // Needed while processing

    { TExecutionRessources }

    TExecutionRessources = class
      IsMultipleDstFolderStrategy :boolean;
      IsDstFileRenamingStrategy :boolean;
      SrcFilenames :array of string;    // After Shaking
      MrkImages :array of TBGRABitmap;  // Empty, One or for each Size
      DstFolders :array of string;      // One or for each Size
      destructor Destroy; override;
    end;

    { TResampleTask }

    TResampleTask = class(TCustomTask)
    protected
      function Execute(Context :TContext) :boolean; override;
      function GetTaskSteps :integer; override;
    public
      ImgRes :TImgRes;
      ExeRes :TExecutionRessources;
      SrcFilename :string;
      SrcFileIndex :integer;
    end;

  private
    FParams :TParams;
    FCancel :boolean;
    FOnPrint :TPrintEvent;
    FOnProgress :TProgressEvent;
    function GetDstFiletemplate: string;
    function GetSizes: string;
    function GetSrcFilenames: TStrings;
    procedure SetDstFiletemplate(AValue: string);
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
    function ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
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
    property SrcFilenames :TStrings read GetSrcFilenames write SetSrcFilenames;
    property DstFolder :string read FParams.DstFolder write FParams.DstFolder;
    property DstFiletemplate :string read GetDstFiletemplate write SetDstFiletemplate;
    property Sizes :string read GetSizes write SetSizes;
    property JpgQuality :integer read FParams.JpgQuality write SetJpgQuality;
    property PngCompression :integer read FParams.PngCompression write SetPngCompression;
    property MrkFilename :string read FParams.MrkFilename write SetMrkFilename; // if msFile
    property MrkSize :single read FParams.MrkSize write SetMrkSize;
    property MrkX :single read FParams.MrkX write SetMrkX;
    property MrkY :single read FParams.MrkY write SetMrkY;
    property MrkAlpha :single read FParams.MrkAlpha write SetMrkAlpha;
    property ThreadCount :integer read FParams.ThreadCount write SetThreadCount;
    property StopOnError :boolean read FParams.StopOnError write FParams.StopOnError;
    property RenEnabled :boolean read FParams.Ren.Enabled;
    property Shake :boolean read FParams.Shake write FParams.Shake;
    property ShakeSeed :integer read FParams.ShakeSeed write SetShakeSeed;
    property Tags :TTags read FParams.Tags write FParams.Tags;
    property Copyright :string read FParams.Copyright write FParams.Copyright;
    property OnPrint :TPrintEvent read FOnPrint write FOnPrint;
    property OnProgress :TProgressEvent read FOnProgress write FOnProgress;
  end;

function TrySizesStrToSizes(const Str :string; out Values :TSizes) :boolean;
function SizesToSizesStr(const Sizes :TSizes) :string;

implementation

uses
  Math, ZStream, FPWriteJpeg, FPWritePng, FPImage, utils,
  generics.collections;

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
  SWrnNoExifFmt = 'No EXIF data found in ''%s''';
  SWrnUpsamplingFmt = 'Upsampling ''%s''';
  SMsgResamplingFmt = 'Resampling ''%s'' from %dx%d to %dx%d...';
  SMsgWatermarkingFmt = 'Watermarking ''%s''...';
  SMsgSavingFmt = 'Saving ''%s''...';
  SMsgLoadingFmt = 'Loading ''%s''...';
  SMsgCreatingFolderFmt = 'Creating folder ''%s''...';
  SMsgShakingFiles = 'Shaking list of files...';
  SMsgLoadMrkFileFmt = 'Loading Watermark ''%s''...';
  SErrInvalidThreadCount = 'Invalid threadcount.';
  SErrMissingSizes = 'Missing sizes.';
  SErrInvalidSizesFmt = 'Invalid sizes ''%s''.';
  SErrMultipleSizes = 'Multiple sizes but placeholder %SIZE% not found in either folder or filename template.';
  SErrCopyright = 'Cant transfer copyright AND override - check Tagging options.';
  SErrInvalidPngCompressionFmt = 'Invalid png compression %d (0..3 expected).';
  SErrInvalidJpgQualityFmt = 'Invalid JPEG quality %d (1..100 expected).';
  SErrInvalidRenamingParamFmt = 'Invalid renaming parameter ''%s''.';
  SErrInvalidINDEXPlaceholderFmt = 'Invalid INDEX placeholder parameters ''%s''.';
  SErrInvalidINDEXStartFmt = 'Invalid INDEX start ''%s''.';
  SErrInvalidINDEXDigitsFmt = 'Invalid INDEX digits number ''%s''.';
  SErrInvalidINDEXParamCountFmt = 'Invalid INDEX parameter count ''%s'' (2 expected).';
  SErrInvalidPlaceholder = 'Unknown or invalid placeholder.';
  SInfResultFmt = 'Images: %d, Sizes: %d, Tasks: %d, Successful: %d, Failed: %d, Elapsed: %.2fs';

const
  PNGCOMPRS :array[0..3] of string = (SCptPNGCompNone, SCptPNGCompFastest, SCptPNGCompDefault, SCptPNGCompMax);
  LEVELSTRS :array[TLevel] of string = (SCptHint, '', SCptWarning, SCptAbort, SCptFatal);

type
  TIntegerArrayHelper = specialize TArrayHelper<Integer>;

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

{ TImgRes.TExecutionRessources }

destructor TImgRes.TExecutionRessources.Destroy;
var
  i :integer;
begin
  for i:=0 to High(MrkImages) do
      MrkImages[i].Free;
  inherited Destroy;
end;

{ TImgRes.TResampleTask }

function TImgRes.TResampleTask.Execute(Context: TContext): boolean;
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
  MetaData :TMetaData;
  Tags :TTags;
begin

  SrcImg := nil;
  result := false;

  if Context.Cancelled then
    Exit;

  // Source File
  try
    if Context.Cancelled then Exit;

    // Load source file...
    Print(Format(SMsgLoadingFmt, [SrcFilename]));
    SrcImg := TBGRABitmap.Create(SrcFilename);

    // If Tags transfer required, load EXIF tags
    MetaData.Clear;
    Tags := [];
    if IsJPEG(SrcFilename) then begin
      if ImgRes.FParams.Tags<>[] then begin
        if ReadMetaData(SrcFilename, MetaData) then begin
          Tags := ImgRes.FParams.Tags;
        end else
          Print(Format(SWrnNoExifFmt, [ExtractFilename(SrcFilename)]), mlWarning);
      end;
      if ImgRes.FParams.Copyright<>'' then begin
        include(Tags, ttCopyright);
        MetaData.Copyright := ImgRes.FParams.Copyright;
      end;
    end;

    if Context.Cancelled then Exit;
    Progress(1);

    m := Length(ImgRes.FParams.Sizes);
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
            CompressionQuality := TFPJPEGCompressionQuality(ImgRes.JpgQuality);

        end else if IsPNG(SrcFilename) then begin

          // Png-options
          Writer := TFPWriterPNG.Create;
          with TFPWriterPNG(Writer) do
            CompressionLevel := ZStream.TCompressionLevel(ImgRes.PngCompression);

        end else
          raise Exception.CreateFmt(SErrFormatNotSupportedFmt, [DstFileExt]);

        // Calculate new size
        Size := ImgRes.FParams.Sizes[i];
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
        DstImg := ImgRes.ResampleImg(SrcImg, DstSize);

        ////////////////////////////////////////////////////////////////////////////
        Progress(1);
        if Context.Cancelled then Exit;

        ////////////////////////////////////////////////////////////////////////////
        // Watermark
        if ImgRes.MrkFilename<>'' then begin
          MrkImg := ExeRes.MrkImages[i mod Length(ExeRes.MrkImages)];

          // Watermark size in percent of the width or original size if MrkSize=0.0
          if ImgRes.MrkSize<>0.0 then begin
            MrkRectSize.cx := round(DstSize.cx*ImgRes.MrkSize/100.0);
            MrkRectSize.cy := round(DstSize.cx*ImgRes.MrkSize/100.0 * MrkImg.Height/MrkImg.Width);
          end else begin
            MrkRectSize.cx := MrkImg.Width;
            MrkRectSize.cy := MrkImg.Height;
          end;
          MrkRect.Left := round((DstSize.cx - MrkRectSize.cx) * ImgRes.MrkX/100.0);
          MrkRect.Top := round((DstSize.cy - MrkRectSize.cy) * ImgRes.MrkY/100.0);
          MrkRect.Width := MrkRectSize.cx;
          MrkRect.Height := MrkRectSize.cy;
          Print(Format(SMsgWatermarkingFmt, [ExtractFilename(SrcFilename), SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));
          DstImg.StretchPutImage(MrkRect, MrkImg, dmLinearBlend, round(255*ImgRes.MrkAlpha/100.0));
        end;

        Progress(1);
        if Context.Cancelled then Exit;
        ////////////////////////////////////////////////////////////////////////////

        // Saving...
        if ImgRes.FParams.Ren.Enabled then begin
          // Specialize the prepared template
          // %FILENAME%
          DstFiletitle := ExtractFilename(SrcFilename);
          if Length(DstFileExt)>0 then
            DstFiletitle := Copy(DstFiletitle, 1, Length(DstFiletitle)-Length(DstFileExt)-1);
          // %FILEEXT% - has been extracted previously
          // %INDEX%
          if ImgRes.FParams.Ren.IndexDigits = 0 then
            IndexStr := IntToStr(SrcFileIndex)
          else begin
            if ImgRes.FParams.Ren.IndexDigits = -1 then
              n := round(log10(ImgRes.FParams.SrcFilenames.Count))+1
            else
              n := ImgRes.FParams.Ren.IndexDigits;
            IndexStr := Format('%*.*d', [n, n, SrcFileIndex+ImgRes.FParams.Ren.IndexStart]);
          end;
          // %SIZE%
          SizeStr := IntToStr(Size);
          DstFiletitleExt := Format(ImgRes.FParams.Ren.FmtStr,
            [DstFiletitle, DstFileExt, IndexStr, SizeStr]);
        end else
          DstFiletitleExt := ExtractFilename(SrcFilename);

        // Save
        DstFolder := ExeRes.DstFolders[i mod Length(ExeRes.DstFolders)];
        DstFilename := IncludeTrailingPathDelimiter(DstFolder) + DstFiletitleExt;
        Print(Format(SMsgSavingFmt, [DstFilename]));
        DstImg.SaveToFile(DstFilename, Writer);

        // EXIF
        if Tags<>[] then begin
          WriteMetaData(DstFilename, MetaData, Tags);
        end;

        Progress(1);

      finally
        Writer.Free;
        DstImg.Free;
      end;
    end;
    result := true;
  finally
    SrcImg.Free;
  end;
end;

function TImgRes.TResampleTask.GetTaskSteps: integer;
begin
  result := 1 + 3*Length(ImgRes.FParams.Sizes);
end;

{ TImgRes }

constructor TImgRes.Create;
begin
  FParams.SrcFilenames    := TStringList.Create;
  SetLength(FParams.Sizes, 0);
  FParams.JpgQuality      := DEFAULTJPGQUALITY;
  FParams.PngCompression  := DEFAULTPNGCOMPRESSION;
  FParams.MrkFilename     := '';
  FParams.MrkSize         := DEFAULTMRKSIZE;
  FParams.MrkX            := DEFAULTMRKX;
  FParams.MrkY            := DEFAULTMRKY;
  FParams.MrkAlpha        := DEFAULTMRKALPHA;
  FParams.ThreadCount     := DEFAULT_THREADCOUNT;
  FParams.StopOnError     := DEFAULT_STOPONERROR;
  FParams.Ren.Enabled     := DEFAULT_RENENABLED;
  FParams.Ren.FmtStr      := DEFAULT_RENFMTSTR;
  FParams.Ren.IndexStart  := DEFAULT_RENINDEXSTART;
  FParams.Ren.IndexDigits := DEFAULT_RENINDEXDIGITS;
  FParams.Shake           := DEFAULT_SHAKE;
  FParams.ShakeSeed       := DEFAULT_SHAKESEED;
  FParams.Tags            := DEFAULT_TAGS;
  FParams.Copyright       := DEFAULT_COPYRIGHT;
end;

destructor TImgRes.Destroy;
begin
  FParams.SrcFilenames.Free;
  inherited Destroy;
end;

class function TImgRes.CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
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

procedure TImgRes.Print(const Line: string; Level: TLevel);
begin
  if Assigned(FOnPrint) then begin
    OnTaskPrint(self, -1, Line, Level);
  end;
end;

procedure TImgRes.OnTaskPrint(Sender: TObject; WorkerId: integer; const Line: string; Level: TLevel);
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

procedure TImgRes.OnTaskProgress(Sender: TObject; Progress: single);
begin
  if Assigned(FOnProgress) then begin
    FOnProgress(self, Progress);
    if FCancel then
      (Sender as TDispatcher).Cancel;
  end;
end;

procedure TImgRes.SetThreadCount(AValue: integer);
begin
  if FParams.ThreadCount = AValue then Exit;
  if AValue<-1 then
    raise Exception.Create(SErrInvalidThreadCount);
  FParams.ThreadCount := AValue;
end;

procedure TImgRes.SetShakeSeed(AValue: integer);
begin
  if AValue<0 then AValue := 0;
  FParams.ShakeSeed := AValue;
end;

function TImgRes.ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
begin
  Img.ResampleFilter := rfLanczos2;
  result := Img.Resample(Size.cx, Size.cy) as TBGRABitmap;
end;

function TImgRes.Execute :boolean;
var
  ExeRes :TExecutionRessources;
  Task :TResampleTask;
  Tasks :TTasks;
  Dispatcher :TDispatcher;
  i, j, n, m :integer;
  x :string;
begin
  // Check main parameters
  if Length(FParams.Sizes)=0 then
    raise Exception.Create(SErrMissingSizes);
  if FParams.SrcFilenames.Count=0 then
    Exit(true);

  FCancel           := false;
  ExeRes            := TExecutionRessources.Create;
  Tasks             := TTasks.Create;
  Dispatcher        := TDispatcher.Create;
  try
    // Prepare the destination file renaming feature
    n := FParams.SrcFilenames.Count;
    m := Length(FParams.Sizes);

    // Check, if multiple sizes, then either %SIZE% must be in folder or in renamed filename
    ExeRes.IsDstFileRenamingStrategy := FParams.Ren.Enabled and (Pos('%3:s', FParams.Ren.FmtStr)>0);
    if Length(FParams.Sizes)>1 then begin
      ExeRes.IsMultipleDstFolderStrategy := Pos('%SIZE%', FParams.DstFolder)>0;
      if not ExeRes.IsMultipleDstFolderStrategy and not ExeRes.IsDstFileRenamingStrategy then
        raise Exception.Create(SErrMultipleSizes);
    end else
      ExeRes.IsMultipleDstFolderStrategy := false;

    // EXIF - check if not Copyright shall be transfered AND Copyright has to be overridden
    if (ttCopyright in FParams.Tags) AND (FParams.Copyright<>'') then
      raise Exception.Create(SErrCopyright);

    // If the files list has to be randomly remixed, its the right moment
    SetLength(ExeRes.SrcFilenames, n);
    for i:=0 to n-1 do ExeRes.SrcFilenames[i] := FParams.SrcFilenames[i];
    if FParams.Shake then begin
      Print(SMsgShakingFiles);
      if FParams.ShakeSeed=0 then Randomize else RandSeed := FParams.ShakeSeed;
      for i:=0 to n-1 do begin
        j := Random(n);
        x := ExeRes.SrcFilenames[i];
        ExeRes.SrcFilenames[i] := ExeRes.SrcFilenames[j];
        ExeRes.SrcFilenames[j] := x;
      end;
    end;

    // Load MrkImages...
    if FParams.MrkFilename='' then begin
      SetLength(ExeRes.MrkImages, 0);
    end else begin
      if FParams.MrkFilenameDependsOnSize then begin
        SetLength(ExeRes.MrkImages, m);
        for i:=0 to m-1 do begin
          x := ReplaceStr(FParams.MrkFilename, '%SIZE%', IntToStr(FParams.Sizes[i]));
          Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(x)]));
          ExeRes.MrkImages[i] := TBGRABitmap.Create(x);
        end;
      end else begin
        SetLength(ExeRes.MrkImages, 1);
        Print(Format(SMsgLoadMrkFileFmt, [ExtractFilename(FParams.MrkFilename)]));
        ExeRes.MrkImages[0] := TBGRABitmap.Create(FParams.MrkFilename);
      end;
    end;

    // Create Destination Folders...
    SetLength(ExeRes.DstFolders, m);
    for i:=0 to m-1 do begin
      x := ReplaceStr(FParams.DstFolder, '%SIZE%', IntToStr(FParams.Sizes[i]));
      ExeRes.DstFolders[i] := x;
    end;
    for i:=0 to High(ExeRes.DstFolders) do begin
      Print(Format(SMsgCreatingFolderFmt, [ExeRes.DstFolders[i]]));
      ForceDirectories(ExeRes.DstFolders[i]);
    end;

    // For each SrcFilename create a task
    Tasks.Capacity := n;
    for i:=0 to n-1 do begin
      Task := TResampleTask.Create;
      Task.ImgRes := self;
      Task.ExeRes := ExeRes;
      Task.SrcFilename := ExeRes.SrcFilenames[i];
      Task.SrcFileIndex := i;
      Tasks.Add(Task);
    end;

    Dispatcher.OnPrint := @OnTaskPrint;
    Dispatcher.OnProgress := @OnTaskProgress;
    Dispatcher.MaxWorkerCount := ThreadCount;
    Dispatcher.StopOnError := StopOnError;

    result := Dispatcher.Execute(Tasks);

    if Assigned(FOnPrint) then with Dispatcher.Stats do
      FOnPrint(self, Format(SInfResultFmt, [n, m, TaskCount, Successful, Failed, Elapsed/1000.0]));

  finally
    Dispatcher.Free;
    Tasks.Free;
    ExeRes.Free;
  end;

end;

procedure TImgRes.Cancel;
begin
  FCancel := true;
end;

class function TImgRes.TryStrToPngCompression(const Str: string; out Value: integer): boolean;
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

class function TImgRes.PngCompressionToStr(const Value :integer) :string;
begin
  if (Value<0) or (Value>High(PNGCOMPRS)) then
    raise Exception.CreateFmt(SErrInvalidPngCompressionFmt, [Value]);
  result := PNGCOMPRS[Value];
end;

class function TImgRes.TryStrToJpgQuality(const Str :string; out Value :integer) :boolean;
begin
  if SameText(Trim(Str), SCptJpgQualityDefault) then begin
    Value := DEFAULTJPGQUALITY;
    result := true;
  end else
    result := TryStrToInt(Str, Value) and (Value>=1) and (Value<=100);
end;

class function TImgRes.JpgQualityToStr(const Value :integer) :string;
begin
  if (Value<1) or (Value>100) then
    raise Exception.CreateFmt(SErrInvalidJpgQualityFmt, [Value]);
  if Value=DEFAULTJPGQUALITY then
    result := SCptJpgQualityDefault
  else
    result := IntToStr(Value);
end;

class function TImgRes.TryStrToRenameParams(const Str: string; out Params: TRenameParams; out ErrStr: string): boolean;
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

class function TImgRes.RenameParamsToStr(const Params: TRenameParams): string;
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

procedure TImgRes.SetSizes(AValue :string);
begin
  if not TrySizesStrToSizes(AValue, FParams.Sizes) then
    raise Exception.Create(Format(SErrInvalidSizesFmt, [AValue]));
end;

function TImgRes.GetSizes: string;
begin
  result := SizesToSizesStr(FParams.Sizes);
end;

function TImgRes.GetDstFiletemplate: string;
begin
  result := RenameParamsToStr(FParams.Ren);
end;

function TImgRes.GetSrcFilenames: TStrings;
begin
  result := FParams.SrcFilenames;
end;

procedure TImgRes.SetDstFiletemplate(AValue: string);
var
  ErrMsg :string;
  Params :TRenameParams;
begin
  if not TryStrToRenameParams(AValue, Params, ErrMsg) then
    raise Exception.Create(ErrMsg);
  FParams.Ren := Params;
end;

procedure TImgRes.SetMrkFilename(AValue: string);
begin
  if AValue=FParams.MrkFilename then Exit;
  FParams.MrkFilename := AValue;
  FParams.MrkFilenameDependsOnSize := Pos('%SIZE%', AValue)>0;
end;

procedure TImgRes.SetJpgQuality(AValue: integer);
begin
  if FParams.JpgQuality=AValue then Exit;
  if (AValue<1) or (AValue>100) then
    raise Exception.CreateFmt(SErrInvalidJpgQualityFmt, [AValue]);
  FParams.JpgQuality:=AValue;
end;

procedure TImgRes.SetPngCompression(AValue: integer);
begin
  if FParams.PngCompression=AValue then Exit;
  if (AValue<0) or (AValue>3) then
    raise Exception.CreateFmt(SErrInvalidPNGCompressionFmt, [AValue]);
  FParams.PngCompression:=AValue;
end;

procedure TImgRes.SetMrkSize(AValue: single);
begin
  if FParams.MrkSize=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FParams.MrkSize:=AValue;
end;

procedure TImgRes.SetMrkX(AValue: single);
begin
  if FParams.MrkX=AValue then Exit;
  FParams.MrkX:=AValue;
end;

procedure TImgRes.SetMrkY(AValue: single);
begin
  if FParams.MrkY=AValue then Exit;
  FParams.MrkY:=AValue;
end;

procedure TImgRes.SetMrkAlpha(AValue: single);
begin
  if FParams.MrkAlpha=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FParams.MrkAlpha:=AValue;
end;

procedure TImgRes.SetSrcFilenames(AValue: TStrings);
begin
  FParams.SrcFilenames.Assign(AValue);
end;

class function TImgRes.GetVersion: string;
begin
  result := IMGRESVER;
end;

initialization
begin
  Randomize;
end;

end.

