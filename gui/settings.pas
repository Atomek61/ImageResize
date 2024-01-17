unit settings;

{$mode delphi}

interface

uses
  Classes, SysUtils, IniFiles, Generics.Collections, Logging, IniFilesHelper,
  Translations, StringArrays;

type
  TSettings = class;

  TSettingClass = class of TSetting;

  { TSetting }

  TSetting = class
  private
    FCaption: string;
    FSettings :TSettings;
    FKey :string;
    FTextDefault :string;
  protected
    procedure Changed;
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    function GetDisplay :string; virtual;
    procedure SetDisplay(const AValue :string); virtual;
    procedure SetText(AValue: string); virtual; abstract;
    function GetText :string; virtual; abstract;
  public
    class procedure Register(SettingClass :TSettingClass);
    class function ClassDefault :string; virtual; abstract;
    constructor Create(Settings :TSettings; const Key :string); virtual;
    function Compare(Setting :TSetting) :boolean; virtual;
    procedure Assign(Source :TSetting); virtual;
    procedure SetDefault;
    property Caption :string read FCaption;
    property Key :string read FKey;
    property Text :string read GetText write SetText;
    property Display :string read GetDisplay write SetDisplay;
    property TextDefault :string read FTextDefault write FTextDefault;
  end;

  { TStringSetting }

  TStringSetting = class(TSetting)
  private
    FValue :string;
  protected
    procedure SetText(AValue: string); override;
    function GetText :string; override;
  public
    class function ClassDefault :string; override;
    function Compare(Setting :TSetting) :boolean; override;
    property Value :string read FValue write SetText;
  end;

  { TIntegerSetting }

  TIntegerSetting = class(TSetting)
  private
    FValue :integer;
    FMin :integer;
    FMax :integer;
    procedure SetValue(AValue: integer);
  protected
    procedure Load(Ini :TCustomIniFile; const Section :string); override;
    procedure SetText(AValue: string); override;
    function GetText :string; override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function Compare(Setting :TSetting) :boolean; override;
    property Value :integer read FValue write SetValue;
    property Min :integer read FMin write FMin;
    property Max :integer read FMax write FMax;
  end;

  { TBooleanSetting }

  TBooleanSetting = class(TSetting)
  private
    FValue :boolean;
    FDisplays :array[boolean] of string;
    procedure SetValue(AValue: boolean);
  protected
    procedure Load(Ini :TCustomIniFile; const Section :string); override;
    procedure SetText(AValue: string); override;
    function GetText :string; override;
    function GetDisplay :string; override;
    procedure SetDisplay(const AValue :string); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function Compare(Setting :TSetting) :boolean; override;
    property Value :boolean read FValue write SetValue;
  end;

  TSettingList = TObjectList<TSetting>;
  TSettingDictionary = TDictionary<string, TSetting>;

  TSettingsClass = class of TSettings;

  { TSettings }

  TSettings = class
  private
    FKey :string; // The Ini-Section
    FLanguage :string;
    FDirty :boolean;
    FOnChanged :TNotifyEvent;
    FSettingList :TSettingList;
    FSettingDict :TSettingDictionary;
  protected
    procedure Changed;
  public
    class procedure Register(Value :TSettingsClass);
    constructor Create(const Key :string); virtual; overload;
    destructor Destroy; override;
    procedure SetDefaults; virtual;
    function Compare(const Value :TSettings) :boolean; virtual;
    procedure Assign(const Source :TSettings); virtual;
    procedure Load(Ini :TCustomIniFile; const Section :string);
    procedure Add(Setting :TSetting);
    procedure SaveToIni(Ini :TCustomIniFile); virtual;
    procedure LoadFromIni(Ini :TCustomIniFile); virtual;
//    property Section :string read FSection write FSection;
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
    procedure SetDefaults;
    procedure LoadFromIni(Ini :TCustomIniFile);
    procedure SaveToIni(Ini :TCustomIniFile);
    property SectionPrefix :string read FSectionPrefix {write FSectionPrefix whynot};
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property ChangedSettings :TSettings read FChangedSettings;
    property Dirty :boolean read FDirty;
  end;

implementation

resourcestring
  SCptTrue = 'True';
  SCptFalse = 'False';
  SErrSettingClassNotFoundFmt = 'Setting class ''%s'' from [''%s''] not registered.';
  SErrConvertToIntegerFmt = 'Cant convert ''%s'' to integer.';
  SErrConvertToBooleanFmt = 'Cant convert ''%s'' to boolean.';
  SErrInvalidNumberOfValuesFmt = 'Invalid number of values ''%s'', %d expected.';
  SErrAssigningFmt = 'Cant assign an object of class ''%s'' to an object of class ''%s''.';

var
  SettingClasses :TDictionary<string, TSettingClass>;
  SettingsClasses :TDictionary<string, TSettingsClass>;

const
  BOOLEANSTRINGS :array[boolean] of string = ('False', 'True');

function LastItemAfterDot(const Str :string) :string;
var
  i :integer;
begin
  for i:=Length(Str) downto 1 do
    if Str[i] = '.' then
      Exit(Copy(Str, i+1, Length(Str)-i));
  result := Str;
end;

{ TSettings }

procedure TSettings.Changed;
begin
  FDirty := true;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

constructor TSettings.Create(const Key :string);
begin
  FKey := Key;
  FLanguage := GetLanguageID.LanguageCode;
  FSettingList := TSettingList.Create;
  FSettingDict := TSettingDictionary.Create;
  SetDefaults;
end;

destructor TSettings.Destroy;
begin
  FSettingDict.Free;
  FSettingList.Free;
  inherited Destroy;
end;

class procedure TSettings.Register(Value: TSettingsClass);
begin
  SettingsClasses.Add(Value.ClassName, Value);
end;

//class function TSettings.Create(const Id: string): TSettings;
//var
//  ClassName :string;
//  SettingsClass :TSettingsClass;
//begin
//  ClassName := Format('T%sSettings', [Id]);
//  if not SettingsClasses.TryGetValue(ClassName, SettingsClass) then
//    raise Exception.CreateFmt('Settings class ''%s'' not registered.', [ClassName]);
//  result := SettingsClass.Create;
////Log('%s %s', [ClassName, result.ClassName], llHint);
//  result.FSection := Id;
//end;
//
procedure TSettings.SetDefaults;
var
  Setting :TSetting;
begin
  for Setting in FSettingList do
    Setting.SetDefault;
end;

function TSettings.Compare(const Value: TSettings): boolean;
var
  i :integer;
begin
  result := FSettingList.Count = Value.FSettingList.Count;
  if result then begin
    for i:=0 to FSettingList.Count-1 do
      if not FSettingList[i].Compare(Value.FSettingList[i]) then Exit;
  end;
end;

procedure TSettings.Assign(const Source: TSettings);
begin
  if ClassType<>Source.ClassType then
    raise Exception.CreateFmt(SErrAssigningFmt, [Source.ClassName, ClassName]);
end;

procedure TSettings.Load(Ini: TCustomIniFile; const Section: string);
var
  Sections :TStringList;
  SectionDot :string;
  SettingSection :string;
  Setting :TSetting;
  SettingKey :string;
  SettingClassName :string;
  SettingClass :TSettingClass;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    SectionDot := Section+'.';
    for SettingSection in Sections do
      if SettingSection.StartsWith(SectionDot, true) then begin
        SettingKey := Copy(SettingSection, Length(SectionDot)+1, Length(SettingSection)-Length(SectionDot)-1);
        SettingClassName := 'T'+Ini.ReadString(SettingSection, 'Class', '')+'Setting';
        if not SettingClasses.TryGetValue(SettingClassName, SettingClass) then
          raise Exception.CreateFmt(SErrSettingClassNotFoundFmt, [SettingClassName, SettingSection]);
        Setting := SettingClass.Create(self, SettingKey);
        try
          Setting.Load(Ini, SettingSection);
          Add(Setting);
        except
          Setting.Free;
          raise;
        end;
      end;
  finally
    Sections.Free;
  end;
end;

procedure TSettings.Add(Setting: TSetting);
begin
  FSettingDict.Add(Setting.Key, Setting);
  FSettingList.Add(Setting);
end;

procedure TSettings.SaveToIni(Ini: TCustomIniFile);
begin
  FDirty := false;
end;

procedure TSettings.LoadFromIni(Ini: TCustomIniFile);
begin
  FDirty := false;
end;

constructor TSetting.Create(Settings: TSettings; const Key :string);
begin
  FKey := Key;
  FSettings := Settings;
  FTextDefault := ClassDefault;
end;

function TSetting.Compare(Setting: TSetting): boolean;
begin
  result := (Key = Setting.Key) and (self.ClassType = Setting.ClassType);
end;

procedure TSetting.Assign(Source: TSetting);
begin
  if ClassType <> Source.ClassType then
    raise Exception.CreateFmt(SErrAssigningFmt, [Source.ClassName, self.ClassName]);
  Text := Source.Text;
end;

procedure TSetting.SetDefault;
begin
  Text := FTextDefault;
end;

procedure TSetting.Load(Ini: TCustomIniFile; const Section :string);
begin
  FKey := LastItemAfterDot(Section);
  FCaption := Ini.ReadLang(Section, 'Caption', FSettings.Flanguage);
  FTextDefault := Ini.ReadString(Section, 'Default', ClassDefault);
  Text := Ini.ReadLang(Section, 'Text', FSettings.Flanguage);
end;

procedure TSetting.Changed;
begin
  FSettings.Changed;
end;

function TSetting.GetDisplay: string;
begin
  result := Text;
end;

procedure TSetting.SetDisplay(const AValue: string);
begin
  Text := AValue;
end;

class procedure TSetting.Register(SettingClass: TSettingClass);
begin
  SettingClasses.Add(SettingClass.ClassName, SettingClass);
end;

{ TStringSetting }

procedure TStringSetting.SetText(AValue: string);
begin
  if AValue=Value then Exit;
  FValue := AValue;
  Changed;
end;

function TStringSetting.GetText: string;
begin
  result := FValue;
end;

class function TStringSetting.ClassDefault: string;
begin
  result := '';
end;

function TStringSetting.Compare(Setting: TSetting): boolean;
begin
  result := inherited Compare(Setting) and (Value = TStringSetting(Setting).Value);
end;

{ TIntegerSetting }

procedure TIntegerSetting.SetValue(AValue: integer);
begin
  if FValue=AValue then Exit;
  FValue := AValue;
  Changed;
end;

procedure TIntegerSetting.Load(Ini: TCustomIniFile; const Section: string);
begin
  inherited Load(Ini, Section);
  FMin := Ini.ReadInteger(Section, 'Min', low(integer));
  FMax := Ini.ReadInteger(Section, 'Min', high(integer));
end;

procedure TIntegerSetting.SetText(AValue: string);
var
  v :integer;
begin
  if not TryStrToInt(AValue, v) then
    raise EConvertError.CreateFmt(SErrConvertToIntegerFmt, [AValue]);
  if v<FMin then
    v := FMin
  else
    if v>FMax then v := FMax;
  if v=FValue then Exit;
  FValue := v;
  Changed;
end;

function TIntegerSetting.GetText: string;
begin
  result := IntToStr(FValue);
end;

constructor TIntegerSetting.Create(Settings: TSettings; const Key: string);
begin
  inherited Create(Settings, Key);
  FMin := Low(Integer);
  FMax := High(Integer);
end;

class function TIntegerSetting.ClassDefault: string;
begin
  result := '0';
end;

function TIntegerSetting.Compare(Setting: TSetting): boolean;
begin
  result := inherited Compare(Setting) and (Value = TIntegerSetting(Setting).Value);
end;

{ TBooleanSetting }

procedure TBooleanSetting.SetValue(AValue: boolean);
begin
  if AValue=Value then Exit;
  FValue := AValue;
  Changed;
end;

procedure TBooleanSetting.Load(Ini: TCustomIniFile; const Section: string);
var
  s :string;
  sa :TStringArray;
begin
  inherited Load(Ini, Section);
  s := Ini.ReadLang(Section, 'Displays', FDisplays[False]+','+FDisplays[True], FSettings.FLanguage);
  sa := s.Split(',');
  if sa.Count<>2 then
    raise Exception.CreateFmt(SErrInvalidNumberOfValuesFmt, [s, 2]);
  FDisplays[False] := sa[0];
  FDisplays[True] := sa[1];
end;

procedure TBooleanSetting.SetText(AValue: string);
begin
  if SameText(AValue, BOOLEANSTRINGS[True]) then
    Value := True
  else if SameText(AValue, BOOLEANSTRINGS[False]) then
    Value := False
  else if SameText(AValue, '1') then
    Value := True
  else if SameText(AValue, '0') then
    Value := False
  else
    raise EConvertError.CreateFmt(SErrConvertToBooleanFmt, [AValue]);
end;

function TBooleanSetting.GetText: string;
begin
  if Value then
    result := BOOLEANSTRINGS[True]
  else
    result := BOOLEANSTRINGS[False];
end;

function TBooleanSetting.GetDisplay: string;
begin
  if Value then
    result := SCptTrue
  else
    result := SCptFalse;
end;

procedure TBooleanSetting.SetDisplay(const AValue: string);
begin
  if SameText(AValue, SCptTrue) then
    Value := True
  else if SameText(AValue, SCptFalse) then
    Value := False
  else if SameText(AValue, '1') then
    Value := True
  else if SameText(AValue, '0') then
    Value := False
  else
    raise EConvertError.CreateFmt(SErrConvertToBooleanFmt, [AValue]);
end;

constructor TBooleanSetting.Create(Settings: TSettings; const Key :string);
begin
  inherited;
  FDisplays[False] := SCptFalse;
  FDisplays[True] := SCptTrue;
end;

class function TBooleanSetting.ClassDefault: string;
begin
  result := BOOLEANSTRINGS[False];
end;

function TBooleanSetting.Compare(Setting: TSetting): boolean;
begin
  result := inherited Compare(Setting) and (Value = TBooleanSetting(Setting).Value);
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

procedure TSettingsList.SetDefaults;
var
  Settings :TSettings;
begin
  for Settings in self.Values do
    Settings.SetDefaults;
end;

procedure TSettingsList.LoadFromIni(Ini: TCustomIniFile);
var
  Sections :TStrings;
  Section :string;
  Settings :TSettings;
  Key :string;
  ClassId :string;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
      if Section.StartsWith(SectionPrefix+'.') then begin
        Key := Copy(Section, Length(SectionPrefix)+2, Length(Section)-Length(SectionPrefix)-1);
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
  SettingClasses := TDictionary<string, TSettingClass>.Create;
  TSetting.Register(TStringSetting);
end;

finalization
begin
  SettingClasses.Free;
  SettingsClasses.Free;
end;

end.



