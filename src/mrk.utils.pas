unit mrk.utils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Types, Graphics, BGRABitmap, BGRATextFX, BGRABitmapTypes, BGRAGradients,
  IniFiles;

const
  WATERMARKPARAMSVERSION = 200;
  WATERMARKLASTSETTINGS = 'mrksettings';
  WATERMARKFAVORITE = 'favorite';

type

  { TWatermarkParams }

  TWatermarkParams = record
    Text :string;
    FontSize :integer;
    FontName :string;
    FontStyle :TFontStyles;
    FontColor :TColor;
    Shadow :boolean;
    ShadowBlur :integer;
    ShadowColor :TColor;
    ShadowQuality :integer;
    Outline :boolean;
    OutlineColor :TColor;
    OutlineWidth :integer;
    procedure Defaults;
    procedure SaveToIni(Ini :TIniFile);
    procedure LoadFromIni(Ini :TIniFile);
  end;

function TryCreateWatermarkImage(const Params :TWatermarkParams; var Img :TBGRABitmap) :boolean;
function TryStrToFontInfo(const Str :string; out FontSize :INTEGER; out FontName :string; out FontStyle :TFontStyles) :boolean;
function FontInfoToStr(FontSize :integer; const FontName :string; FontStyle :TFontStyles) :string;

implementation

uses
  utils;

function TryCreateWatermarkImage(const Params :TWatermarkParams; var Img :TBGRABitmap) :boolean;
var
  Renderer: TBGRATextEffectFontRenderer;
  Size :TSize;
begin
  Img := TBGRABitmap.Create(0, 0, BGRAPixelTransparent);
  Renderer := TBGRATextEffectFontRenderer.Create;
  Renderer.ShadowOffset := Point(0, 0);
  Renderer.ShadowRadius := Params.ShadowBlur;
  Renderer.ShadowVisible := Params.Shadow;
  Renderer.ShadowColor := Params.ShadowColor;
  Renderer.ShadowQuality := TRadialBlurType(Params.ShadowQuality);
  Renderer.OutlineVisible := Params.Outline;
  Renderer.OutlineColor := Params.OutlineColor;
  Renderer.OutlineWidth := Params.OutlineWidth;
  Renderer.OuterOutlineOnly := True;
  Img.FontRenderer := renderer;
  Img.FontName := Params.FontName;
  Img.FontStyle := Params.FontStyle;
  Img.FontHeight := Params.FontSize;
  Size := Img.TextSize(Params.Text);
  Img.SetSize(Size.cx+2*Params.ShadowBlur, Size.cy+2*Params.ShadowBlur);
  Img.TextOut(Params.ShadowBlur, Params.ShadowBlur, Params.Text, Params.FontColor);
  result := true;
end;

function TryStrToFontInfo(const Str :string; out FontSize :integer; out FontName :string; out FontStyle :TFontStyles) :boolean;
var
  Items :TStringArray;
  i :integer;
begin
  Items := Str.Split(',');
  if (Length(Items)<2) or (Length(Items)>4) then Exit(false);
  // Remove Whitespace
  for i:=0 to High(Items) do
    Items[i] := RemoveQuotes(Trim(Items[i]));
  if not SameText(RightStr(Items[0], 2), 'pt') then Exit(false);
  if not TryStrToInt(LeftStr(Items[0], Length(Items[0])-2), FontSize) then Exit(False);
  FontName := Items[1];
  FontStyle := [];
  for i:=2 to High(Items) do
    if SameText(Trim(Items[i]), 'italic') then
      FontStyle := FontStyle + [fsItalic]
    else if SameText(Trim(Items[i]), 'bold') then
      FontStyle := FontStyle + [fsBold]
    else Exit(false);
  result := true;
end;

function FontInfoToStr(FontSize :integer; const FontName :string; FontStyle :TFontStyles) :string;
begin
  result := Format('%dPt, "%s"', [FontSize, FontName]);
  if fsBold in FontStyle then
    result := result + ', bold';
  if fsItalic in FontStyle then
    result := result + ', italic';
end;

{ TWatermarkParams }

procedure TWatermarkParams.Defaults;
var y, m, d :word;
begin
  DecodeDate(Now, y, m, d);
  Text := Format('© %d ', [y]);
  FontName := 'Arial';
  FontSize := 48;
  FontStyle := [fsBold];
  FontColor := clWhite;
  Shadow := true;
  ShadowBlur := 6;
  ShadowColor := clNavy;
  ShadowQuality := 0;
  Outline := true;
  OutlineColor := clBlack;
  OutlineWidth := 4;
end;

procedure TWatermarkParams.SaveToIni(Ini: TIniFile);
begin
  Ini.WriteString('Settings', 'Text', Text);
  Ini.WriteString('Settings', 'Font', FontInfoToStr(FontSize, FontName, FontStyle));
  Ini.WriteString('Settings', 'FontColor', '$'+Format('%6.6x', [UInt32(FontColor)]));
  Ini.WriteBool('Settings', 'Shadow', Shadow);
  Ini.WriteInteger('Settings', 'ShadowBlur', ShadowBlur);
  Ini.WriteString('Settings', 'ShadowColor', '$'+Format('%6.6x', [UInt32(ShadowColor)]));
  Ini.WriteInteger('Settings', 'ShadowQuality', ShadowQuality);
  Ini.WriteBool('Settings', 'Outline', Outline);
  Ini.WriteString('Settings', 'OutlineColor', '$'+Format('%6.6x', [UInt32(OutlineColor)]));
  Ini.WriteString('Settings', 'OutlineWidth', IntToStr(OutlineWidth));
end;

procedure TWatermarkParams.LoadFromIni(Ini: TIniFile);
var
  Item :string;
begin
  Defaults;
  Text := Ini.ReadString('Settings', 'Text', Text);
  Item := Ini.ReadString('Settings', 'Font', FontInfoToStr(FontSize, FontName, FontStyle));
  TryStrToFontInfo(Item, FontSize, FontName, FontStyle);
  FontColor := Ini.ReadInteger('Settings', 'FontColor', UInt32(FontColor));
  Shadow := Ini.ReadBool('Settings', 'Shadow', Shadow);
  ShadowBlur := Ini.ReadInteger('Settings', 'ShadowBlur', ShadowBlur);
  ShadowColor := Ini.ReadInteger('Settings', 'ShadowColor', UInt32(ShadowColor));
  ShadowQuality := Ini.ReadInteger('Settings', 'ShadowQuality', ShadowQuality);
  Outline := Ini.ReadBool('Settings', 'Outline', Outline);
  OutlineColor := Ini.ReadInteger('Settings', 'OutlineColor', UInt32(OutlineColor));
  OutlineWidth := Ini.ReadInteger('Settings', 'OutlineWidth', UInt32(OutlineWidth));
end;

end.

