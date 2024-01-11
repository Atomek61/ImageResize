unit presentationsettings;

{$mode delphi}

interface

uses
  Classes, SysUtils, Settings, IniFiles, Generics.Collections;

type

  { TPresentationSettings }

  TPresentationSettings = class(TSettings)
  private
    FTitle :string;
    procedure SetTitle(AValue: string);
  public
    procedure Defaults; override;
    function Compare(const Value :TSettings) :boolean; override;
    procedure Assign(const Value :TSettings); override;
    procedure SaveToIni(Ini :TCustomIniFile); override;
    procedure LoadFromIni(Ini :TCustomIniFile); override;
    property Title :string read FTitle write SetTitle;
  end;

  TSlideshow200Settings = class(TPresentationSettings)

  end;

  { TSimple100Settings }

  TSimple100Settings = class(TPresentationSettings)
  public
    constructor Create; override;
  end;

const
  PRESENTATIONSETTINGS_PREFIX = 'Presentation';

implementation

{ TPresentationSettings }

procedure TPresentationSettings.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Changed;
end;

procedure TPresentationSettings.Defaults;
begin
  FTitle := '';
end;

function TPresentationSettings.Compare(const Value: TSettings): boolean;
var
  Settings :TPresentationSettings;
begin
  Settings := Value as TPresentationSettings;
  result := FTitle= Settings.Title;
end;

procedure TPresentationSettings.Assign(const Value: TSettings);
var
  Settings :TPresentationSettings;
begin
  Settings := Value as TPresentationSettings;
  Title := Settings.Title;
end;

procedure TPresentationSettings.SaveToIni(Ini: TCustomIniFile);
begin
  inherited;
  with Ini do begin
    WriteString(Section,'Title', Title);
  end;
end;

procedure TPresentationSettings.LoadFromIni(Ini: TCustomIniFile);
var
  Value :string;

  function Read(const Name :string) :boolean;
  begin
    Value := Ini.ReadString(Section, Name, '?');
    result := Value<>'?';
  end;

begin
  with Ini do begin
    if Read('Title') then Title := Value;
  end;
  inherited;
end;

{ TSimple100Settings }

constructor TSimple100Settings.Create;
begin
  inherited Create;
end;

initialization
begin
  TSettings.Register(TPresentationSettings);
  TSettings.Register(TSimple100Settings);
end;

end.

