unit imgutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

const
  LANDSCAPESTR      = 'Landscape';
  PORTRAITSTR       = 'Portrait';
  ICONSTR           = 'Icon';

type
  TImageRatio = (irCustom, ir11, ir43, ir32, ir169);
  TImageOrientation = (ioLandscape, ioPortrait, ioIcon);

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
  LANDSCAPESTR, PORTRAITSTR, ICONSTR);

function WidthHeightToRatioAndOrientation(w, h: integer; out Ratio: TImageRatio; out Orientation: TImageOrientation): boolean;
var
  x :integer;
  r100 :integer;
begin
  result := (w>0) or (h>0);
  if result then begin
    if w>h then Orientation := ioLandscape else if w<h then Orientation := ioPortrait else Orientation := ioIcon;
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

end.

