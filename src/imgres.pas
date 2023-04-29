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
  Classes, SysUtils, StrUtils, Types, SyncObjs, BGRABitmap, BGRABitmapTypes,
  threading.dispatcher;

const
  IMGRESVER = '2.7';
  IMGRESCPR = 'imgres V'+IMGRESVER+' © 2023 Jan Schirrmacher, www.atomek.de';

  PROGRESSSTEPSPERFILE = 4;

  IMGRESREGKEY = 'SOFTWARE\Atomek\Image Resize\';

const
//  DEFAULTSIZE            = 640;
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

  //// Watermark sources
  //msDisabled  = 0;
  //msFile      = 1;
  //msImage     = 2;
  //
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
      MrkImage :TBGRABitmap;
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
    end;
  private type
    TResampleTask = class;

    // Cached objects while execution, shared by the workers. This is, that
    // Bitmaps are loaded only once, not by every Worker

    { TSharedRessources }

    TSharedRessources = class
    private
      FImgRes :TImgRes;
      FSrcImgsSection :TCriticalSection;
      FSrcImgs :array of TBGRABitmap;
      FSrcImgsRefCount :array of integer; // Increases on each use/unuse, when all Taskas are done - then the Imgage can be freed
      FMrkImgsSection :TCriticalSection;
      FMrkImgs :array of TBGRABitmap; // cached for each size
      FDstFoldersSection :TCriticalSection;
      FDstFolders :array of string;   // cached for each size
    public
      constructor Create(AImgRes :TImgRes);
      destructor Destroy; override;
      function RequestSrcImg(Task :TResampleTask) :TBGRABitmap;
      procedure ReleaseSrcImg(Task :TResampleTask);
      function GetMrkImg(Task :TResampleTask) :TBGRABitmap;
      function GetDstFolder(Task :TResampleTask) :string;
    end;

    { TResampleTask }

    TResampleTask = class(TCustomTask)
    protected
      function Execute(Context :TContext) :boolean; override;
      function GetTaskSteps :integer; override;
    public
      SharedRessources :TSharedRessources;
      SrcFilenameIndex :integer;
      SizeIndex :integer;
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
  PNGCOMPRS :array[0..3] of string = ('none', 'fastest', 'default', 'max');

type
  TIntegerArrayHelper = specialize TArrayHelper<Integer>;

procedure ShakeSequence(List :TStrings);
var
  n, i :integer;
begin
  n := List.Count;
  for i:=0 to n-1 do
    List.Exchange(i, Random(n));
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

{ TImgRes.TResampleTask }

function TImgRes.TResampleTask.Execute(Context: TContext): boolean;
var
  SrcImg :TBGRABitmap;
  SrcFilename :string;
  DstFolder :string;
  DstFileExt :string;
  DstFileExtU :string;
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
  ImgRes :TImgRes;
  IndexStr :string; // Index to display
  SizeStr :string;
//  IndexDigits :integer;
  n :integer;
begin
  Writer := nil;
  DstImg := nil;
  result := false;
  ImgRes := SharedRessources.FImgRes;

  if Context.Cancelled then
    Exit;

  // Destination Folder
  DstFolder := SharedRessources.GetDstFolder(self);

  // Source File
  SrcImg := SharedRessources.RequestSrcImg(self);
  try
    SrcFilename := ImgRes.SrcFilenames[SrcFilenameIndex];

    if Context.Cancelled then
      Exit;
    Progress(1);

    // Create Writer depending on file extension
    DstFileExt := ExtractFileExt(SrcFilename);
    if Length(DstFileExt)>0 then DstFileExt := Copy(DstFileExt, 2, Length(DstFileExt)-1);
    DstFileExtU := UpperCase(DstFileExt);
    if (DstFileExtU = 'JPG') or (DstFileExtU = 'JPEG') then begin

      // Jpg-options
      Writer := TFPWriterJPEG.Create;
      with TFPWriterJPEG(Writer) do
        CompressionQuality := TFPJPEGCompressionQuality(ImgRes.JpgQuality);

    end else if DstFileExtU = 'PNG' then begin

      // Png-options
      Writer := TFPWriterPNG.Create;
      with TFPWriterPNG(Writer) do
        CompressionLevel := ZStream.TCompressionLevel(ImgRes.PngCompression);
    end else
      raise Exception.CreateFmt('Format %s not supported.', [DstFileExt]);

    // Calculate new size
    Size := ImgRes.FParams.Sizes[SizeIndex];
    SrcSize := TSize.Create(SrcImg.Width, SrcImg.Height);
    DstSize := CalcResamplingSize(SrcSize, Size);

    // Warning, if upsampling
    if (SrcSize.cx<DstSize.cx) or (DstSize.cy<DstSize.cy) then
      Print(Format('Upsampling ''%s''', [
         ExtractFilename(SrcFilename)]), mlWarning);

    ////////////////////////////////////////////////////////////////////////////
    // Resampling...
    Print(Format('Resampling ''%s'' from %dx%d to %dx%d...', [
      ExtractFilename(SrcFilename), SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));
    DstImg := SharedRessources.FImgRes.ResampleImg(SrcImg, DstSize);
    ////////////////////////////////////////////////////////////////////////////
    if Context.Cancelled then
      Exit;
    Progress(1);

    ////////////////////////////////////////////////////////////////////////////
    // Watermark
    if ImgRes.MrkFilename<>'' then begin
      MrkImg := SharedRessources.GetMrkImg(self);

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
      Print(Format('Watermarking ''%s''...', [
        ExtractFilename(SrcFilename), SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));
      DstImg.StretchPutImage(MrkRect, MrkImg, dmLinearBlend, round(255*ImgRes.MrkAlpha/100.0));
    end;
    if Context.Cancelled then
      Exit;
    Progress(1);
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
        IndexStr := IntToStr(SrcFilenameIndex)
      else begin
        if ImgRes.FParams.Ren.IndexDigits = -1 then
          n := round(log10(ImgRes.FParams.SrcFilenames.Count))+1
        else
          n := ImgRes.FParams.Ren.IndexDigits;
        IndexStr := Format('%*.*d', [n, n, SrcFilenameIndex+ImgRes.FParams.Ren.IndexStart]);
      end;
      // %SIZE%
      SizeStr := IntToStr(Size);
      DstFiletitleExt := Format(ImgRes.FParams.Ren.FmtStr,
        [DstFiletitle, DstFileExt, IndexStr, SizeStr]);
    end else
      DstFiletitleExt := ExtractFilename(SrcFilename);

    DstFilename := IncludeTrailingPathDelimiter(DstFolder) + DstFiletitleExt;
    Print(Format('Saving ''%s''...' , [DstFilename]));
    DstImg.SaveToFile(DstFilename, Writer);
    Progress(1);

    result := true;
  finally
    Writer.Free;
    DstImg.Free;
    SharedRessources.ReleaseSrcImg(self)
  end;
end;

function TImgRes.TResampleTask.GetTaskSteps: integer;
begin
  result := PROGRESSSTEPSPERFILE; // Loading, Resampling, Watermarking, Saving
end;

{ TImgRes.TSharedRessources }

constructor TImgRes.TSharedRessources.Create(AImgRes: TImgRes);
var
  i :integer;
begin
  FImgRes := AImgRes;
  FSrcImgsSection := TCriticalSection.Create;
  SetLength(FSrcImgs, FImgRes.SrcFilenames.Count);
  SetLength(FSrcImgsRefCount, FImgRes.SrcFilenames.Count);
  for i:=0 to High(FSrcImgsRefCount) do
    FSrcImgsRefCount[i] := Length(AImgRes.FParams.Sizes);
  FMrkImgsSection := TCriticalSection.Create;
  if FImgRes.FParams.MrkFilenameDependsOnSize then
    SetLength(FMrkImgs, Length(FImgRes.FParams.Sizes))
  else
    SetLength(FMrkImgs, 1);
  FDstFoldersSection := TCriticalSection.Create;
  SetLength(FDstFolders, Length(FImgRes.FParams.Sizes));
end;

destructor TImgRes.TSharedRessources.Destroy;
var
  i :integer;
begin
  for i:=0 to High(FSrcImgs) do
    FSrcImgs[i].Free;
  for i:=0 to High(FMrkImgs) do
    FMrkImgs[i].Free;
  FSrcImgsSection.Free;
  FMrkImgsSection.Free;
  FDstFoldersSection.Free;
  inherited Destroy;
end;

function TImgRes.TSharedRessources.RequestSrcImg(Task :TResampleTask): TBGRABitmap;
var
  SrcFilename :string;
begin
  FSrcImgsSection.Enter;
  try
    SrcFilename := FImgRes.FParams.SrcFilenames[Task.SrcFilenameIndex];
    if not Assigned(FSrcImgs[Task.SrcFilenameIndex]) then begin
      Task.Print(Format('Loading ''%s''...', [ExtractFilename(SrcFilename)]));
      FSrcImgs[Task.SrcFilenameIndex] := TBGRABitmap.Create(SrcFilename);
      ;
    end;
    result := FSrcImgs[Task.SrcFilenameIndex];
  finally
    FSrcImgsSection.Leave;
  end;
end;

procedure TImgRes.TSharedRessources.ReleaseSrcImg(Task: TResampleTask);
begin
  FSrcImgsSection.Enter;
  try
    dec(FSrcImgsRefCount[Task.SrcFilenameIndex]);
    if FSrcImgsRefCount[Task.SrcFilenameIndex]<=0 then
      FreeAndNil(FSrcImgs[Task.SrcFilenameIndex]);
  finally
    FSrcImgsSection.Leave;
  end;
end;

function TImgRes.TSharedRessources.GetMrkImg(Task :TResampleTask): TBGRABitmap;
var
  Filename :string;
  Index :integer;
begin
  FMrkImgsSection.Enter;
  try
    if FImgRes.FParams.MrkFilenameDependsOnSize then
      Index := Task.SizeIndex
    else
      Index := 0;
    if not Assigned(FMrkImgs[Index]) then begin
      if FImgRes.FParams.MrkFilenameDependsOnSize then
        Filename := ReplaceStr(FImgRes.FParams.MrkFilename, '%SIZE%', IntToStr(FImgRes.FParams.Sizes[Index]))
      else
        Filename := FImgRes.FParams.MrkFilename;
      Task.Print(Format('Loading ''%s''...', [ExtractFilename(Filename)]));
      FMrkImgs[Index] := TBGRABitmap.Create(Filename);
    end;
    result := FMrkImgs[Index]
  finally
    FMrkImgsSection.Leave;
  end;
end;

function TImgRes.TSharedRessources.GetDstFolder(Task :TResampleTask): string;
var
  DstFolder :string;
  Index :integer;
begin
  FDstFoldersSection.Enter;
  try
    Index := Task.SizeIndex;
    if FDstFolders[Index]='' then begin
      DstFolder := ReplaceStr(FImgRes.FParams.DstFolder, '%SIZE%', IntToStr(FImgRes.FParams.Sizes[Index]));
      Task.Print(Format('Creating folder ''%s''...', [DstFolder]));
      ForceDirectories(DstFolder);
      FDstFolders[Index] := DstFolder;
    end;
    result := FDstFolders[Index];
  finally
    FDstFoldersSection.Leave;
  end;
end;

{ TImgRes }

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

procedure TImgRes.OnTaskPrint(Sender: TObject; WorkerId: integer; const Line: string; Level: TLevel);
const
  LEVELSTRS :array[TLevel] of string = ('Hint - ', '', 'Warning - ', 'Abort - ', 'Fatal - ');
begin
  if Assigned(FOnPrint) then begin
    FOnPrint(self, Format('[%d] %s%s', [WorkerId+1, LEVELSTRS[Level], Line]));
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
    raise Exception.Create('Invalid threadcount.');
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

constructor TImgRes.Create;
begin
  FParams.SrcFilenames    := TStringList.Create;
  SetLength(FParams.Sizes, 0);
  FParams.JpgQuality      := DEFAULTJPGQUALITY;
  FParams.PngCompression  := DEFAULTPNGCOMPRESSION;
  FParams.MrkFilename     := '';
  FParams.MrkImage        := TBGRABitmap.Create;
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
end;

destructor TImgRes.Destroy;
begin
  FParams.SrcFilenames.Free;
  FParams.MrkImage.Free;
  inherited Destroy;
end;

function TImgRes.Execute :boolean;
var
  SharedRessources :TSharedRessources;
  Task :TResampleTask;
  Tasks :TTasks;
  Dispatcher :TDispatcher;
  i, j, n, m :integer;
begin
  // Check main parameters
  if Length(FParams.Sizes)=0 then
    raise Exception.Create('Missing sizes.');
  if FParams.SrcFilenames.Count=0 then
    Exit(true);

  // Check, if multiple sizes, then either %SIZE% must be in folder or in renamed filename
  if Length(FParams.Sizes)>1 then begin
    if not ((Pos('%SIZE%', FParams.DstFolder)>0) or (FParams.Ren.Enabled and (Pos('%3:s', FParams.Ren.FmtStr)>0))) then
      raise Exception.Create('Multiple sizes, placeholder %SIZE% not found in either folder or filename template.');
  end;

  FCancel           := false;
  SharedRessources  := TSharedRessources.Create(self);
  Tasks             := TTasks.Create;
  Dispatcher        := TDispatcher.Create;
  try
    // Prepare the destination file renaming feature
    n := FParams.SrcFilenames.Count;
    m := Length(FParams.Sizes);

    // If the files list hasto be randomly remixed, its the right moment
    if FParams.Shake then begin
      if FParams.ShakeSeed=0 then Randomize else RandSeed := FParams.ShakeSeed;
      ShakeSequence(FParams.SrcFilenames);
    end;

    // For each SrcFilename/Size create a task
    Tasks.Capacity := n*m;
    for i:=0 to n-1 do
      for j:=0 to m-1 do begin
        Task := TResampleTask.Create;
        Task.SharedRessources := SharedRessources;
        Task.SrcFilenameIndex := i;
        Task.SizeIndex := j;
        Tasks.Add(Task);
      end;

    Dispatcher.OnPrint := @OnTaskPrint;
    Dispatcher.OnProgress := @OnTaskProgress;
    Dispatcher.MaxWorkerCount := ThreadCount;
    Dispatcher.StopOnError := StopOnError;

    result := Dispatcher.Execute(Tasks);

    if Assigned(FOnPrint) then with Dispatcher.Stats do
      FOnPrint(self, Format('Images: %d, Sizes: %d, Tasks:%d, Successful:%d, Failed:%d, Elapsed:%.2fs',
        [n, m, TaskCount, Successful, Failed, Elapsed/1000.0]));

  finally
    Dispatcher.Free;
    Tasks.Free;
    SharedRessources.Free;
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
    raise Exception.CreateFmt('Invalid png compression %d (0..3).', [Value]);
  result := PNGCOMPRS[Value];
end;

class function TImgRes.TryStrToJpgQuality(const Str :string; out Value :integer) :boolean;
begin
  if SameText(Trim(Str), 'default') then begin
    Value := DEFAULTJPGQUALITY;
    result := true;
  end else
    result := TryStrToInt(Str, Value) and (Value>=1) and (Value<=100);
end;

class function TImgRes.JpgQualityToStr(const Value :integer) :string;
begin
  if (Value<1) or (Value>100) then
    raise Exception.CreateFmt('Invalid jpg quality %d (1..100).', [Value]);
  if Value=DEFAULTJPGQUALITY then
    result := 'default'
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
    Exit(Err(Format('Invalid renaming parameter ''%s''.', [Str])));

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
        Exit(Err(Format('Invalid INDEX placeholder parameters ''%s''.', [Placeholders[i]])));
      if (Length(Items)>0) then begin
        if not (TryStrToInt(Items[0], Params.IndexStart)) or (Params.IndexStart<0) then
          Exit(Err(Format('Invalid INDEX start ''%s''.', [Items[0]])));
        if (Length(Items)>1) then begin
          // auto, 0, 1, ...
          if SameText(Items[1], 'AUTO') then
            Params.IndexDigits := -1
          else if not (TryStrToInt(Items[1], Params.IndexDigits)) or (Params.IndexDigits<0) then
            Exit(Err(Format('Invalid INDEX digits number ''%s''.', [Items[1]])));
        end else if (Length(Items)>2) then
          Exit(Err(Format('Invalid INDEX parameter count ''%s'' (2 expected).', [Placeholders[i]])));
      end;
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%'+Placeholders[i]+'%', '%2:s');
    end;
    if IsPlaceholder('SIZE', i) then begin
      Params.FmtStr := ReplaceStr(Params.FmtStr, '%SIZE%', '%3:s');
    end;
    if ParamCount<Length(Placeholders) then
      Exit(Err('Unknown or invalid placeholder.'));
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
    raise Exception.Create(Format('Invalid sizes ''%s''.', [AValue]));
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
    raise Exception.CreateFmt('Invalid jpg quality %d (1..100).', [AValue]);
  FParams.JpgQuality:=AValue;
end;

procedure TImgRes.SetPngCompression(AValue: integer);
begin
  if FParams.PngCompression=AValue then Exit;
  if (AValue<0) or (AValue>3) then
    raise Exception.CreateFmt('Invalid png compression %d (0..3).', [AValue]);
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

