unit settings;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, Generics.Collections, Logging;

type

  TSettingsClass = class of TSettings;
  { TSettings }

  TSettings = class
  private
    FDirty :boolean;
    FOnChanged :TNotifyEvent;
    FSection :string;
  protected
    procedure Changed;
  public
    constructor Create; virtual; overload;
    class procedure Register(Value :TSettingsClass);
    class function Create(const Id :string) :TSettings; overload;
    procedure Defaults; virtual;
    function Compare(const Value :TSettings) :boolean; virtual;
    procedure Assign(const Value :TSettings); virtual;
    procedure SaveToIni(Ini :TCustomIniFile); virtual;
    procedure LoadFromIni(Ini :TCustomIniFile); virtual;
    property Section :string read FSection write FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TSettingsList }

  TSettingsList = class(TObjectDictionary<string, TSettings>)
  private
    FSectionPrefix :string;
  public
    constructor Create(const SectionPrefix :string; OwnsObjects :boolean);
    procedure LoadFromIni(Ini :TCustomIniFile);
    procedure SaveToIni(Ini :TCustomIniFile);
    property SectionPrefix :string read FSectionPrefix {write FSectionPrefix whynot};
  end;

implementation

var
  SettingsClasses :TDictionary<string, TSettingsClass>;

{ TSettings }

procedure TSettings.Changed;
begin
  FDirty := true;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

constructor TSettings.Create;
begin
  FSection := Copy(ClassName, 2, Length(ClassName)-9);
  Defaults;
end;

class procedure TSettings.Register(Value: TSettingsClass);
begin
  SettingsClasses.Add(Value.ClassName, Value);
end;

class function TSettings.Create(const Id: string): TSettings;
var
  ClassName :string;
  SettingsClass :TSettingsClass;
begin
  ClassName := Format('T%sSettings', [Id]);
  if not SettingsClasses.TryGetValue(ClassName, SettingsClass) then
    raise Exception.CreateFmt('Settings class ''%s'' not registered.', [ClassName]);
  result := SettingsClass.Create;
//Log('%s %s', [ClassName, result.ClassName], llHint);
  result.FSection := Id;
end;

procedure TSettings.Defaults;
begin
end;

function TSettings.Compare(const Value: TSettings): boolean;
begin
  result := false;
end;

procedure TSettings.Assign(const Value: TSettings);
begin

end;

procedure TSettings.SaveToIni(Ini: TCustomIniFile);
begin
  FDirty := false;
end;

procedure TSettings.LoadFromIni(Ini: TCustomIniFile);
begin
  FDirty := false;
end;

{ TSettingsList }

constructor TSettingsList.Create(const SectionPrefix: string; OwnsObjects: boolean);
const
  OWNERSHIP :array[boolean] of TDictionaryOwnerships = ([], [doOwnsValues]);
begin
  inherited Create(OWNERSHIP[OwnsObjects]);
  FSectionPrefix := SectionPrefix;
end;

procedure TSettingsList.LoadFromIni(Ini: TCustomIniFile);
var
  Sections :TStrings;
  Section :string;
  Settings :TSettings;
  Id :string;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
      if Section.StartsWith(SectionPrefix+'.') then begin
        Id := Copy(Section, Length(SectionPrefix)+2, Length(Section)-Length(SectionPrefix)-1);
        if not TryGetValue(Id, Settings) then begin
          Settings := TSettings.Create(Id);
          Add(Id, Settings);
        end;
        Settings.LoadFromIni(Ini);
      end;
  finally
    Sections.Free;
  end;
end;

procedure TSettingsList.SaveToIni(Ini: TCustomIniFile);
var
  Settings :TSettings;
begin
  for Settings in Values do
    Settings.SaveToIni(Ini);
end;

initialization
begin
  SettingsClasses := TDictionary<string, TSettingsClass>.Create;
end;

finalization
begin
  SettingsClasses.Free;
end;

end.



