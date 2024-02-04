unit colorsetting;

{$mode Delphi}

interface

uses
  Classes, SysUtils, IniFiles, Graphics, Settings, WebUtils;

type

  { THTMLColorSetting }

  THTMLColorSetting = class(TSetting)
  private
    FValue :TColor;
    FDefault :TColor;
    procedure SetValue(AValue: TColor);
  protected
    procedure SetAsText(const AValue: string); override;
    function GetAsText :string; override;
    procedure SetDefault; override;
  public
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
    property Value :TColor read FValue write SetValue;
    property Default :TColor read FDefault write FDefault;
  end;

implementation

resourcestring
  SErrConvertToHTMLColorFmt = 'Cant convert ''%s'' to a HTMLColor.';

{ THTMLColorSetting }

procedure THTMLColorSetting.SetValue(AValue: TColor);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  Change;
end;

procedure THTMLColorSetting.SetAsText(const AValue: string);
var
  Value :TColor;
begin
  if not TryHTMLColorToColor(AValue, Value) then
    raise EConvertError.CreateFmt(SErrConvertToHTMLColorFmt, [AValue]);
  self.Value := Value;
end;

function THTMLColorSetting.GetAsText: string;
begin
  result := ColorToHTMLColor(FValue);
end;

procedure THTMLColorSetting.SetDefault;
begin
  Value := FDefault;
end;

procedure THTMLColorSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FDefault := HTMLColorToColor(Ini.ReadString(Section, 'Default', '#000000'));
  FValue := FDefault;
end;

constructor THTMLColorSetting.Create(Settings: TSettings; const Key: string);
begin
  inherited;
  FValue := 0;
  FDefault := 0;
end;

class function THTMLColorSetting.ClassDefault: string;
begin
  result := '0';
end;

function THTMLColorSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (FValue = THTMLColorSetting(Other).FValue);
end;

procedure THTMLColorSetting.Assign(Source: TSetting);
begin
  inherited;
  if not IsEqual(Source) then begin
    FValue := THTMLColorSetting(Source).FValue;
    Change;
  end;
end;

initialization
begin
  TSetting.Register([THTMLColorSetting]);
end;

end.

