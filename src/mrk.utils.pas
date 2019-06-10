unit mrk.utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, strutils, Graphics, BGRABitmap, BGRABitmapTypes, BGRAGradients;

type
  TWatermarkImageParams = record
    Width :integer;
    Text :string;
    FontName :string;
    FontStyle :TFontStyles;
    FontColor :TColor;
    ShadowBlur :integer;
    ShadowColor :TColor;
  end;

function TryCreateWatermarkImage(const Params :TWatermarkImageParams; out Img :TBGRABitmap) :boolean;
function TryStrToFontInfo(const Str :string; out FontName :string; out FontStyle :TFontStyles) :boolean;
function FontInfoToStr(const FontName :string; FontStyle :TFontStyles) :string;

implementation

uses
  utils;

function TryCreateWatermarkImage(const Params :TWatermarkImageParams; out Img :TBGRABitmap) :boolean;
var
  Height :integer;
  TextSize :TSize;
  d :integer;
  FontHeight :integer;
  FontPixel :TBGRAPixel;
  ShadowPixel :TBGRAPixel;
  Tmp :TBGRABitmap;
begin

  // Get an idea of the texts size
  Tmp := TBGRABitmap.Create(1, 1);
  try
    with Params do begin
      Tmp.FontHeight := 20;
      Tmp.FontName := FontName;
      Tmp.FontStyle := FontStyle;
    end;
    TextSize := Tmp.TextSize(Params.Text);
    TextSize.cx := round(TextSize.cx*1.25);
  finally
    Tmp.Free;
  end;

  // Calc images size
  d := Params.ShadowBlur;
  FontHeight := round((Params.Width-2*d)*TextSize.cy/TextSize.cx);
  Height := round(FontHeight + 2*d);

  FontPixel.FromColor(Params.FontColor);
  ShadowPixel.FromColor(Params.ShadowColor);
  Img := TextShadow(Params.Width, Height, Params.Text, FontHeight,
    FontPixel, ShadowPixel, 0, 0, Params.ShadowBlur, Params.FontStyle, Params.FontName);
  result := true;
end;

function TryStrToFontInfo(const Str :string; out FontName :string; out FontStyle :TFontStyles) :boolean;
var
  Items :TStringArray;
  i :integer;
begin
  result := StrToStringArray(Str, ',', Items) and (Length(Items)>0) and (Length(Items)<=3);
  if result then begin
    FontName := RemoveQuotes(Items[0]);
    FontStyle := [];
    for i:=1 to High(Items) do
      if SameText(Trim(Items[i]), 'italic') then
        FontStyle := FontStyle + [fsItalic]
      else if SameText(Trim(Items[i]), 'bold') then
        FontStyle := FontStyle + [fsBold]
      else Exit(false);
  end;
end;

function FontInfoToStr(const FontName :string; FontStyle :TFontStyles) :string;
begin
  result := Format('"%s"', [FontName]);
  if fsBold in FontStyle then
    result := result + ', bold';
  if fsItalic in FontStyle then
    result := result + ', italic';
end;

end.

