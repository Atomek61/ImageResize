unit mrk.utils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Types, Graphics, BGRABitmap, BGRABitmapTypes, BGRAGradients,
  IniFiles;

const
  WATERMARKPARAMSVERSION = 100;
  WATERMARKSECTION = 'Watermark';
  WATERMARKDEFAULTSECTION = 'Default Watermark';

type

  { TWatermarkParams }

  TWatermarkParams = record
    Width :integer;
    Text :string;
    FontName :string;
    FontStyle :TFontStyles;
    FontColor :TColor;
    ShadowBlur :integer;
    ShadowColor :TColor;
    procedure Defaults;
    procedure SaveToIni(Ini :TCustomIniFile; const Key :string = WATERMARKSECTION);
    procedure LoadFromIni(Ini :TCustomIniFile; const Key :string = WATERMARKSECTION);
  end;

function TryCreateWatermarkImage(const Params :TWatermarkParams; var Img :TBGRABitmap) :boolean;
function TryStrToFontInfo(const Str :string; out FontName :string; out FontStyle :TFontStyles) :boolean;
function FontInfoToStr(const FontName :string; FontStyle :TFontStyles) :string;

implementation

uses
  utils;

function TryCreateWatermarkImage(const Params :TWatermarkParams; var Img :TBGRABitmap) :boolean;
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
  FreeAndNil(Img);
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

{ TWatermarkParams }

procedure TWatermarkParams.Defaults;
begin
  Width := 200;
  Text := '© 2019 Your Name';
  FontName := 'Courier New';
  FontStyle := [fsBold];
  FontColor := clWhite;
  ShadowBlur := 6;
  ShadowColor := clNavy;
end;

procedure TWatermarkParams.SaveToIni(Ini: TCustomIniFile; const Key :string);
begin
  Ini.WriteInteger(Key, 'Width', Width);
  Ini.WriteString(Key, 'Text', Text);
  Ini.WriteString(Key, 'Font', FontInfoToStr(FontName, FontStyle));
  Ini.WriteString(Key, 'FontColor', '$'+Format('%6.6x', [UInt32(FontColor)]));
  Ini.WriteInteger(Key, 'ShadowBlur', ShadowBlur);
  Ini.WriteString(Key, 'ShadowColor', '$'+Format('%6.6x', [UInt32(ShadowColor)]));
end;

procedure TWatermarkParams.LoadFromIni(Ini: TCustomIniFile; const Key :string);
var
  Item :string;
begin
  Defaults;
  Width := Ini.ReadInteger(Key, 'Width', Width);
  Text := Ini.ReadString(Key, 'Text', Text);
  Item := Ini.ReadString(Key, 'Font', FontInfoToStr(FontName, FontStyle));
  TryStrToFontInfo(Item, FontName, FontStyle);
  FontColor := Ini.ReadInteger(Key, 'FontColor', UInt32(FontColor));
  ShadowBlur := Ini.ReadInteger(Key, 'ShadowBlur', ShadowBlur);
  ShadowColor := Ini.ReadInteger(Key, 'ShadowColor', UInt32(ShadowColor));
end;

end.

