unit imgres;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, BGRABitmap, BGRABitmapTypes{, BGRATextFX};

const
  IMGRESVER = '1.2';
  IMGRESCPR = 'imgres V'+IMGRESVER+' © 2019 Jan Schirrmacher, www.atomek.de';

  PROGRESSSTEPSPERFILE = 3;

const
  DEFAULTSIZE           = 640;
  DEFAULTPNGCOMPRESSION = 2;
  DEFAULTJPGQUALITY     = 75;
  DEFAULTMRKSIZE        = 20.0;
  DEFAULTMRKX           = 98.0;
  DEFAULTMRKY           = 98.0;
  DEFAULTMRKALPHA       = 50.0;

type

  { TImgRes }

  TPrintEvent = procedure(Sender :TObject; const Line :string) of object;
  TCanProgressEvent = procedure(Sender :TObject; Progress :single; var Cancel :boolean) of object;

  TImgRes = class
  private
    FSize :integer;
    FJpgQuality :integer;
    FPngCompression :integer;
    FMrkFilename :string; // Watermark filename, may contain %SIZE%
    FMrkSize :single;
    FMrkX :single;
    FMrkY :single;
    FMrkCfn :string; // Current Filename
    FMrkImg :TBGRABitmap;
    FMrkAlpha :single;
    FOnPrint :TPrintEvent;
    FOnCanProgress :TCanProgressEvent;
    FStep :integer;
    FStepCount :integer;
    FCancel :boolean;
    FSrcImg :TBGRABitmap;
    FSrcFilename :string;
    procedure SetJpgQuality(AValue: integer);
    procedure SetMrkAlpha(AValue: single);
    procedure SetMrkSize(AValue: single);
    procedure SetMrkX(AValue: single);
    procedure SetMrkY(AValue: single);
    procedure SetPngCompression(AValue: integer);
    procedure SetSize(AValue: integer);
    function Resample(const SourceFilename, DestinationFolder :string; Size :integer) :boolean;
    function ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
    class function CalcResamplingSize(const Size :TSize; LongWidth :integer) :TSize;
    function CanProgress :boolean;
    procedure FreeCached;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetVersion: string;
    function Execute(const SourceFilename, DestinationFolder :string) :boolean; overload;
    function Execute(SourceFilenames :TStrings; const DestinationFolder :string) :boolean; overload;
    function Execute(SourceFilenames :TStrings; const DestinationFolder :string; const Sizes :TIntegerDynArray) :boolean; overload;
    class function TryStrToPngCompression(const Str :string; out Value :integer) :boolean;
    class function PngCompressionToStr(const Value :integer) :string;
    property Size :integer read FSize write SetSize;
    property JpgQuality :integer read FJpgQuality write SetJpgQuality;
    property PngCompression :integer read FPngCompression write SetPngCompression;
    property WatermarkFilename :string read FMrkFilename write FMrkFilename;
    property WatermarkSize :single read FMrkSize write SetMrkSize;
    property WatermarkX :single read FMrkX write SetMrkX;
    property WatermarkY :single read FMrkY write SetMrkY;
    property WatermarkAlpha :single read FMrkAlpha write SetMrkAlpha;
    property OnPrint :TPrintEvent read FOnPrint write FOnPrint;
    property OnCanProgress :TCanProgressEvent read FOnCanProgress write FOnCanProgress;
  end;

function SizesStrToSizes(const Str :string; out Values :TIntegerDynArray) :boolean;

implementation

uses
  FPWriteJpeg, FPWritePng, ZStream, FPImage, strutils;

const
  PNGCOMPRS :array[0..3] of string = ('none', 'fastest', 'default', 'max');

function SizesStrToSizes(const Str :string; out Values :TIntegerDynArray) :boolean;
var
  i, p, n, x :integer;
begin
  SetLength(Values, 0);
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=',') then begin
      if not TryStrToInt(Copy(Str, p, i-p), x) or (x<1) then Exit(false);
      SetLength(Values, n+1);
      Values[n] := x;
      p := i+1;
      inc(n);
    end;
  end;
  result := true;
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

function TImgRes.CanProgress: boolean;
begin
  if Assigned(FOnCanProgress) then
    FOnCanProgress(self, FStep/FStepCount, FCancel);
  result := not FCancel;
end;

procedure TImgRes.FreeCached;
begin
  if Assigned(FSrcImg) then begin
    FreeAndNil(FSrcImg);
    FSrcFilename := '';
  end;
  if Assigned(FMrkImg) then begin
    FreeAndNil(FMrkImg);
    FMrkCfn := '';
  end;
end;

function TImgRes.ResampleImg(Img :TBgraBitmap; const Size :TSize) :TBgraBitmap;
begin
  Img.ResampleFilter := rfLanczos2;
  result := Img.Resample(Size.cx, Size.cy) as TBGRABitmap;
end;

constructor TImgRes.Create;
begin
  FMrkFilename    := '';
  FSize           := DEFAULTSIZE;
  FJpgQuality     := DEFAULTJPGQUALITY;
  FPngCompression := DEFAULTPNGCOMPRESSION;
  FMrkSize        := DEFAULTMRKSIZE;
  FMrkX           := DEFAULTMRKX;
  FMrkY           := DEFAULTMRKY;
  FMrkAlpha       := DEFAULTMRKALPHA;
end;

destructor TImgRes.Destroy;
begin
  FreeAndNil(FSrcImg);
  FreeAndNil(FMrkCfn);
  inherited Destroy;
end;

function TImgRes.Execute(const SourceFilename, DestinationFolder :string) :boolean;
var
  DstFolder :string;
begin
  FCancel := false;
  FStep := 0;
  FStepCount := PROGRESSSTEPSPERFILE;
  DstFolder := ReplaceStr(DestinationFolder, '%SIZE%', IntToStr(Size));
  try
    result := Resample(SourceFilename, DstFolder, Size);
  finally
    FreeCached;
  end;
end;

function TImgRes.Execute(SourceFilenames :TStrings; const DestinationFolder :string) :boolean; overload;
var
  i, n :integer;
  DstFolder :string;
begin
  FCancel := false;
  n := SourceFilenames.Count;
  FStepCount := PROGRESSSTEPSPERFILE*n;
  FStep := 0;
  DstFolder := ReplaceStr(DestinationFolder, '%SIZE%', IntToStr(Size));
  try
    for i:=0 to n-1 do
      if not Resample(SourceFilenames[i], DstFolder, Size) then Exit(false);
  finally
    FreeCached;
  end;
  result := true;
end;

function TImgRes.Execute(SourceFilenames: TStrings; const DestinationFolder: string; const Sizes :TIntegerDynArray): boolean;
var
  i, j, n, m :integer;
  DstFolder :string;
begin
  FCancel := false;
  n := Length(Sizes);
  m := SourceFilenames.Count;
  FStepCount := PROGRESSSTEPSPERFILE*n*m;
  FStep := 0;

  try
    for i:=0 to m-1 do begin
      for j:=0 to n-1 do begin
        DstFolder := ReplaceStr(DestinationFolder, '%SIZE%', IntToStr(Sizes[j]));
        if not Resample(SourceFilenames[i], DstFolder, Sizes[j]) then Exit(false);
      end;
    end;
  finally
    FreeCached;
  end;
  result := true;
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

procedure TImgRes.SetJpgQuality(AValue: integer);
begin
  if FJpgQuality=AValue then Exit;
  if (AValue<1) or (AValue>100) then
    raise Exception.CreateFmt('Invalid jpg quality %d (1..100).', [AValue]);
  FJpgQuality:=AValue;
end;

procedure TImgRes.SetMrkAlpha(AValue: single);
begin
  if FMrkAlpha=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FMrkAlpha:=AValue;
end;

procedure TImgRes.SetMrkSize(AValue: single);
begin
  if FMrkSize=AValue then Exit;
  if AValue<0.0 then AValue := 0.0;
  if AValue>100.0 then AValue := 100.0;
  FMrkSize:=AValue;
end;

procedure TImgRes.SetMrkX(AValue: single);
begin
  if FMrkX=AValue then Exit;
  FMrkX:=AValue;
end;

procedure TImgRes.SetMrkY(AValue: single);
begin
  if FMrkY=AValue then Exit;
  FMrkY:=AValue;
end;

class function TImgRes.GetVersion: string;
begin
  result := IMGRESVER;
end;

procedure TImgRes.SetPngCompression(AValue: integer);
begin
  if FPngCompression=AValue then Exit;
  if (AValue<0) or (AValue>3) then
    raise Exception.CreateFmt('Invalid png compression %d (0..3).', [AValue]);
  FPngCompression:=AValue;
end;

procedure TImgRes.SetSize(AValue: integer);
begin
  if FSize=AValue then Exit;
  if (FSize<1) then
    raise Exception.CreateFmt('Invalid size %d (>0).', [AValue]);
  FSize:=AValue;
end;

function TImgRes.Resample(const SourceFilename, DestinationFolder: string; Size :integer): boolean;
var
  FileExt :string;
  DstFiletitle :string;
  DstFilename :string;
  DstImg :TBGRABitmap;
  Writer :TFPCustomImageWriter;
//  Renderer :TBGRATextEffectFontRenderer;
  SrcSize :TSize;
  DstSize :TSize;
  MrkRectSize :TSize;
  MrkRect :TRect;
  NxtMrkCfn :string;
begin
  Writer := nil;
  DstImg := nil;

  if not Assigned(FSrcImg) then
    FSrcImg := TBGRABitmap.Create;
    FSrcFilename := '';
  try

    // Create Folder
    ForceDirectories(DestinationFolder);

    // Create Writer depending on file extension
    FileExt := LowerCase(ExtractFileExt(SourceFilename));
    if (FileExt = '.jpg') or (FileExt = '.jpeg') then begin

      // Jpg-options
      Writer := TFPWriterJPEG.Create;
      with TFPWriterJPEG(Writer) do begin
        CompressionQuality := TFPJPEGCompressionQuality(JpgQuality);
      end;

    end else if FileExt = '.png' then begin

      // Png-options
      Writer := TFPWriterPNG.Create;
      with TFPWriterPNG(Writer) do begin
        CompressionLevel := ZStream.TCompressionLevel(PngCompression);
      end;
    end else
      raise Exception.CreateFmt('Format %s not supported.', [FileExt]);

    // Loading...
    if Assigned(FOnPrint) then
      FOnPrint(self, Format('Resampling ''%s''', [SourceFilename]));
    // Check, if it is already loaded...
    if SourceFilename<>FSrcFilename then begin
      FSrcImg.LoadFromFile(SourceFilename);
      FSrcFilename := SourceFilename;
    end;
    inc(FStep);
    if not CanProgress then Exit(false);

    // Calculate new size
    SrcSize := TSize.Create(FSrcImg.Width, FSrcImg.Height);
    DstSize := CalcResamplingSize(SrcSize, Size);

    // Resampling...
    DstImg := ResampleImg(FSrcImg, DstSize);

    inc(FStep);
    if not CanProgress then Exit(false);

    // Watermark
    if FMrkFilename<>'' then begin
      if not Assigned(FMrkImg) then begin
        FMrkImg := TBGRABitmap.Create;
        FMrkCfn := '';
      end;
      NxtMrkCfn := ReplaceStr(FMrkFilename, '%SIZE%', IntToStr(Size));
      if FMrkCfn<>NxtMrkCfn then begin
        FMrkCfn := NxtMrkCfn;
        FMrkImg.LoadFromFile(FMrkCfn);
      end;

      // Watermark size inpercent of the width or original size if MrkSize=0.0
      if FMrkSize<>0.0 then begin
        MrkRectSize.cx := round(DstSize.cx*FMrkSize/100.0);
        MrkRectSize.cy := round(DstSize.cx*FMrkSize/100.0 * FMrkImg.Height/FMrkImg.Width);
      end else begin
        MrkRectSize.cx := FMrkImg.Width;
        MrkRectSize.cy := FMrkImg.Height;
      end;
      MrkRect.Left := round((DstSize.cx - MrkRectSize.cx) * FMrkX/100.0);
      MrkRect.Top := round((DstSize.cy - MrkRectSize.cy) * FMrkY/100.0);
      MrkRect.Width := MrkRectSize.cx;
      MrkRect.Height := MrkRectSize.cy;

      DstImg.StretchPutImage(MrkRect, FMrkImg, dmLinearBlend, round(255*FMrkAlpha/100.0));
    end;

    if Assigned(FOnPrint) then
      FOnPrint(self, Format('SrcSize=%dx%d DstSize=%dx%d', [SrcSize.cx, SrcSize.cy, DstSize.cx, DstSize.cy]));

    // Saving...
    DstFiletitle := ExtractFilename(SourceFilename);
    DstFilename := IncludeTrailingPathDelimiter(DestinationFolder) + DstFiletitle;
    if Assigned(FOnPrint) then
      FOnPrint(self, Format('Saving ''%s''' , [DstFilename]));
    DstImg.SaveToFile(DstFilename, Writer);
    if Assigned(FOnPrint) then begin
      FOnPrint(self, 'Ok');
      FOnPrint(self, '');
    end;
    inc(FStep);
    CanProgress;

  finally
    if FSrcImg<>DstImg then
      DstImg.Free;
    Writer.Free;
  end;
  result := true;
end;

end.

