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
    class procedure Register(Value :TSettingsClass);
    class function Create(const Id :string) :TSettings; overload;
    constructor Create; virtual; overload;
    procedure Defaults; virtual;
    function Compare(const Value :TSettings) :boolean; virtual;
    procedure Assign(const Value :TSettings); virtual;
    procedure SaveToIni(Ini :TCustomIniFile); virtual;
    procedure LoadFromIni(Ini :TCustomIniFile); virtual;
    property Section :string read FSection write FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property Dirty :boolean read FDirty;
  end;

  { TSettingsList }

  TSettingsList = class(TObjectDictionary<string, TSettings>)
  private
    FDirty :boolean;
    FSectionPrefix :string;
    FOnChanged :TNotifyEvent;
    FChangedSettings :TSettings; // Which TSettings has been changed when OnChanged
    procedure DoChanged;
    procedure OnSettingsChanged(Sender :TObject);
  protected
    procedure KeyNotify(constref AKey: string; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(const SectionPrefix :string; OwnsObjects :boolean);
    procedure Defaults;
    procedure LoadFromIni(Ini :TCustomIniFile);
    procedure SaveToIni(Ini :TCustomIniFile);
    property SectionPrefix :string read FSectionPrefix {write FSectionPrefix whynot};
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property ChangedSettings :TSettings read FChangedSettings;
    property Dirty :boolean read FDirty;
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

procedure TSettingsList.DoChanged;
begin
  FDirty := true;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

procedure TSettingsList.OnSettingsChanged(Sender: TObject);
begin
  FChangedSettings := Sender as TSettings;
  DoChanged;
end;

procedure TSettingsList.KeyNotify(constref AKey: string; ACollectionNotification: TCollectionNotification);
begin
  if ACollectionNotification=cnAdded then
    self[AKey].OnChanged := self.OnSettingsChanged;
  inherited KeyNotify(AKey, ACollectionNotification);
end;

constructor TSettingsList.Create(const SectionPrefix: string; OwnsObjects: boolean);
const
  OWNERSHIP :array[boolean] of TDictionaryOwnerships = ([], [doOwnsValues]);
begin
  inherited Create(OWNERSHIP[OwnsObjects]);
  FSectionPrefix := SectionPrefix;
end;

procedure TSettingsList.Defaults;
var
  Settings :TSettings;
begin
  for Settings in self.Values do
    Settings.Defaults;
end;

procedure TSettingsList.LoadFromIni(Ini: TCustomIniFile);
var
  Sections :TStrings;
  Section :string;
  Settings :TSettings;
  Id :string;
  ClassId :string;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
      if Section.StartsWith(SectionPrefix+'.') then begin
        Id := Copy(Section, Length(SectionPrefix)+2, Length(Section)-Length(SectionPrefix)-1);
        ClassId := Ini.ReadString(Section, 'Class', Id);
        if not TryGetValue(Id, Settings) then begin
          Settings := TSettings.Create(ClassId);
          Settings.FSection := Section;
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



