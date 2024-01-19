unit imgtags;

{$mode delphi}

interface

uses
  Classes, SysUtils, Logging, vardisplay;

type

  { TImgVarInfo }

  TImgVarInfo = record
    Name :string;
    cls :TVarClass;
    fmt :string;
    function TagKey :string;
  end;

const
  IMGVARINFOS :array[0..4] of TImgVarInfo = (
    (Name: 'Title';        cls: TStringVar;   fmt: ''),
    (Name: 'OrgSize';      cls: TByteSizeVar; fmt: ''),
    (Name: 'OrgWidth';     cls: TIntegerVar;  fmt: ''),
    (Name: 'OrgHeight';    cls: TIntegerVar;  fmt: ''),
    (Name: 'FocalLength';  cls: TFloatVar;    fmt: '%.1f mm')
  );

const
  IMGTITLE_KEY        = 'IMGTITLE';
  IMGCOPYRIGHT_KEY    = 'IMGCOPYRIGHT';
  IMGTOPICS_KEY       = 'IMGTOPICS';
  IMGTIMESTAMP_KEY    = 'IMGTIMESTAMP';
  IMGAUTHOR           = 'IMGAUTHOR';
  IMGORGNAME_KEY      = 'IMGORGNAME';
  IMGGEOLOCATION_KEY  = 'IMGGEOLOCATION';
  IMGLOCATION_KEY     = 'IMGLOCATION';
  IMGORGSIZE_KEY      = 'IMGORGBYTESIZE';
  IMGSIZE_KEY         = 'IMGSIZE';
  IMGWIDTH_KEY        = 'IMGWIDTH';
  IMGHEIGHT_KEY       = 'IMGHEIGHT';
  IMGBYTES_KEY        = 'IMGBYTES';
  IMGORIENTATION_KEY  = 'IMGORIENTATION';
  IMGRATIO_KEY        = 'IMGRATIO';
  IMGEXPOSURE_KEY     = 'IMGEXPOSURE';
  IMGAPERTURE_KEY     = 'IMGAPERTURE';
  IMGFOCALLENGTH_KEY  = 'IMGFOCALLENGTH';
  IMGISO_KEY          = 'IMGISO';
  IMGMAXAPERTURE_KEY  = 'IMGMAXAPERTURE';
  IMGCAMMODEL_KEY     = 'IMGCAMMODEL';
  IMGCAMMAKER_KEY     = 'IMGCAMMAKER';
//
//  IMGVARKEYS :array[0..19] of string = (
//    IMGTITLE_KEY, IMGTOPICS_KEY, IMGTIMESTAMP_KEY, IMGORGNAME_KEY,
//    IMGGEOLOCATION_KEY, IMGLOCATION_KEY, IMGORGSIZE_KEY, IMGSIZE_KEY,
//    IMGWIDTH_KEY, IMGHEIGHT_KEY, IMGBYTES_KEY, IMGORIENTATION_KEY, IMGRATIO_KEY,
//    IMGEXPOSURE_KEY, IMGAPERTURE_KEY, IMGFOCALLENGTH_KEY, IMGISO_KEY,
//    IMGMAXAPERTURE_KEY, IMGCAMMODEL_KEY, IMGCAMMAKER_KEY
//  );

  LANDSCAPESTR      = 'Landscape';
  PORTRAITSTR       = 'Portrait';
  SYMBOLSTR         = 'Symbol';

type
  TImageRatio = (irCustom, ir11, ir43, ir32, ir169);
  TImageOrientation = (ioLandscape, ioPortrait, ioSymbol);

function WidthHeightToRatioAndOrientation(w, h :integer; out Ratio :TImageRatio; out Orientation :TImageOrientation) :boolean;

function TryStrToRatio(const Value :string; out ImageRatio :TImageRatio) :boolean;
function StrToRatio(const Value :string) :TImageRatio;
function RatioToStr(Value :TImageRatio) :string;

function TryStrToOrientation(const Value :string; out ImageOrientation :TImageOrientation) :boolean;
function StrToOrientation(const Value :string) :TImageOrientation;
function OrientationToStr(Value :TImageOrientation) :string;

resourcestring
  SErrInvalidImageRatioFmt = 'Invalid image format ''%s''.';
  SErrInvalidImageOrientationFmt = 'Invalid image orientation ''%s''.';

implementation

const
  IMAGEFORMATS :array[TImageRatio] of string = (
    'Custom', '1/1', '4/3', '3/2', '16/9');

  IMAGEORIENTATIONS :array[TImageOrientation] of string = (
    LANDSCAPESTR, PORTRAITSTR, SYMBOLSTR);

function WidthHeightToRatioAndOrientation(w, h: integer; out Ratio: TImageRatio; out Orientation: TImageOrientation): boolean;
var
  x :integer;
  r100 :integer;
begin
  result := (w>0) or (h>0);
  if result then begin
    if w>h then Orientation := ioLandscape else if w<h then Orientation := ioPortrait else Orientation := ioSymbol;
    if Orientation = ioPortrait then begin
      x := w;
      w := h;
      h := x;
    end;
    r100 := round(w/h*100);
    case r100 of
      178: Ratio := ir169;
      150: Ratio := ir32;
      133: Ratio := ir43;
      100: Ratio := ir11;
      else
        Ratio := irCustom;
    end;
  end;
end;

function TryStrToRatio(const Value: string; out ImageRatio: TImageRatio): boolean;
var
  i :TImageRatio;
begin
  for i:=Low(TImageRatio) to High(TImageRatio) do
    if SameText(IMAGEFORMATS[i], Value) then begin
      ImageRatio := i;
      Exit(True);
    end;
  result := false;
end;

function StrToRatio(const Value: string): TImageRatio;
begin
  if not TryStrToRatio(Value, result) then
    raise Exception.CreateFmt(SErrInvalidImageRatioFmt, [Value]);
end;

function RatioToStr(Value: TImageRatio): string;
begin
  result := IMAGEFORMATS[Value];
end;

function TryStrToOrientation(const Value: string; out ImageOrientation: TImageOrientation): boolean;
var
  i :TImageOrientation;
begin
  for i:=Low(TImageOrientation) to High(TImageOrientation) do
    if SameText(IMAGEORIENTATIONS[i], Value) then begin
      ImageOrientation := i;
      Exit(True);
    end;
  result := false;
end;

function StrToOrientation(const Value: string): TImageOrientation;
begin
  if not TryStrToOrientation(Value, result) then
    raise Exception.CreateFmt(SErrInvalidImageOrientationFmt, [Value]);
end;

function OrientationToStr(Value: TImageOrientation): string;
begin
  result := IMAGEORIENTATIONS[Value];
end;

{ TImgVarInfo }

function TImgVarInfo.TagKey: string;
begin
  result := 'IMG'+UpperCase(Name);
end;

//procedure Test;
//var
//  Ratio :TImageRatio;
//  Orientation :TImageOrientation;
//  s :string;
//begin
//  WidthHeightToRatioAndOrientation(100, 99, Ratio, Orientation);
//  s := Format('Ratio=%s Orientation=%s', [RatioToStr(Ratio), OrientationToStr(Orientation)]);
//end;
//
//initialization
//begin
//  Test;
//end;
//
end.

