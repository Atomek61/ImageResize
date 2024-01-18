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
    FOnChanged :TNotifyEvent;
  protected
    procedure Changed;
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    function GetDisplay :string; virtual;
    procedure SetDisplay(const AValue :string); virtual;
    procedure SetText(const AValue: string); virtual; abstract;
    function GetText :string; virtual; abstract;
  public
    class procedure Register(const Classes :array of TSettingClass);
    class function ClassDefault :string; virtual; abstract;
    constructor Create(Settings :TSettings; const Key :string); virtual;
    function Compare(Setting :TSetting) :boolean; virtual;
    procedure Assign(Source :TSetting); virtual;
    procedure SetDefault;
    property Settings :TSettings read FSettings;
    property Caption :string read FCaption;
    property Key :string read FKey;
    property Text :string read GetText write SetText;
    property Display :string read GetDisplay write SetDisplay;
    property TextDefault :string read FTextDefault write FTextDefault;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TStringSetting }

  TStringSetting = class(TSetting)
  private
    FValue :string;
  protected
    procedure SetText(const AValue: string); override;
    function GetText :string; override;
  public
    class function ClassDefault :string; override;
    function Compare(Setting :TSetting) :boolean; override;
    property Value :string read FValue write SetText;
  end;

  { TPicklistSetting }

  TPicklistSetting = class(TStringSetting)
  public type
    TMode = (pmList, pmText);
  private
    FItemIndex :integer;
    FTextItems :TStringArray;
    FDisplayItems :TStringArray;
    FMode :TMode;
    procedure SetItemIndex(AValue: integer);
  protected
    procedure Load(Ini :TCustomIniFile; const Section :string); override;
    procedure SetText(const AValue: string); override;
    function GetText :string; override;
    function GetDisplay :string; override;
    procedure SetDisplay(const AValue: string); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function StrToMode(const Str :string) :TMode;
    property TextItems :TStringArray read FTextItems;
    property DisplayItems :TStringArray read FDisplayItems;
    property ItemIndex :integer read FItemIndex write SetItemIndex;
    property Mode :TMode read FMode write FMode;
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
    procedure SetText(const AValue: string); override;
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
    procedure SetText(const AValue: string); override;
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

  TSettings = class(TSettingDictionary)
  private
    FSection :string; // The Ini-Section
    FLanguage :string;
    FDirty :boolean;
    FOnChanged :TNotifyEvent;
    FItems :TSettingList;
  protected
    procedure Changed;
    function Add(constref Setting :TSetting) :SizeInt;
  public
    class procedure Register(Value :TSettingsClass);
    constructor Create(const ASection :string = ''); virtual; overload;
    destructor Destroy; override;
    procedure SetDefaults; virtual;
    function Compare(const Value :TSettings) :boolean; virtual;
    procedure Assign(const Source :TSettings); virtual;
    procedure Load(Ini :TCustomIniFile; const Section :string);
    procedure SaveToIni(Ini :TCustomIniFile); virtual;
    procedure LoadFromIni(Ini :TCustomIniFile); virtual;
    property Items :TSettingList read FItems;
    property Section :string read FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property Dirty :boolean read FDirty;
    property Language :string read FLanguage;
  end;

  { TSettingsList }

  TSettingsList = class(TObjectDictionary<string, TSettings>)
  private
    FDirty :boolean;
    FGroup :string; // i.e. [Main.xxxxx] or [Presentation.xxxxx]
    FOnChanged :TNotifyEvent;
    FChangedSettings :TSettings; // Which TSettings has been changed when OnChanged
    procedure DoChanged;
    procedure OnSettingsChanged(Sender :TObject);
  protected
    procedure KeyNotify(constref AKey: string; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(const AGroup :string; OwnsObjects :boolean);
    procedure SetDefaults;
    procedure LoadFromIni(Ini :TCustomIniFile);
    procedure SaveToIni(Ini :TCustomIniFile);
    property Group :string read FGroup {write FSectionPrefix whynot};
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
  SErrItemsCountMismatchFmt = 'Number of ''%s'' display strings mismatch, %d expected';
  SErrAssigningFmt = 'Cant assign an object of class ''%s'' to an object of class ''%s''.';
  SErrInvalidPicklistModeFmt = 'Invalid picklist mode ''%s''.';

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

constructor TSettings.Create(const ASection :string);
begin
  inherited Create;
  if ASection<>'' then
    FSection := ASection
  else
    FSection := Copy(ClassName, 2, Length(ClassName)-9);
  FLanguage := GetLanguageID.LanguageCode;
  FItems := TSettingList.Create(true);
  SetDefaults;
end;

destructor TSettings.Destroy;
begin
  FItems.Free;
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
  for Setting in Items do
    Setting.SetDefault;
end;

function TSettings.Compare(const Value: TSettings): boolean;
var
  i :integer;
begin
  result := Count = Value.Count;
  if result then begin
    for i:=0 to Items.Count-1 do
      if not Items[i].Compare(Value.Items[i]) then Exit;
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
        SettingKey := Copy(SettingSection, Length(SectionDot)+1, Length(SettingSection)-Length(SectionDot));
        SettingClassName := 'T'+Ini.ReadString(SettingSection, 'Class', '')+'Setting';
        if not SettingClasses.TryGetValue(SettingClassName, SettingClass) then
          raise Exception.CreateFmt(SErrSettingClassNotFoundFmt, [SettingClassName, SettingSection]);
        Setting := SettingClass.Create(self, SettingKey);
        try
          Setting.Load(Ini, SettingSection);
        except
          Setting.Free;
          raise;
        end;
      end;
  finally
    Sections.Free;
  end;
end;

function TSettings.Add(constref Setting: TSetting) :SizeInt;
begin
  inherited Add(Setting.Key, Setting);
  result := FItems.Add(Setting);
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
  FSettings.Add(self);
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
//  FKey := LastItemAfterDot(Section);
  FCaption := Ini.ReadLang(Section, 'Caption', FSettings.Flanguage);
  FTextDefault := Ini.ReadString(Section, 'Default', ClassDefault);
  Text := Ini.ReadLang(Section, 'Text', FSettings.Flanguage);
end;

procedure TSetting.Changed;
begin
  FSettings.Changed;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

function TSetting.GetDisplay: string;
begin
  result := Text;
end;

procedure TSetting.SetDisplay(const AValue: string);
begin
  Text := AValue;
end;

class procedure TSetting.Register(const Classes :array of TSettingClass);
var
  AClass :TSettingClass;
begin
  for AClass in Classes do
    SettingClasses.Add(AClass.ClassName, AClass);
end;

{ TStringSetting }

procedure TStringSetting.SetText(const AValue: string);
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

{ TPicklistSetting }

procedure TPicklistSetting.SetItemIndex(AValue: integer);
begin
  if (FItemIndex=AValue) or (FItemIndex<-1) or (FItemIndex>=FTextItems.Count) then Exit;
  if AValue=-1 then
    SetText('')
  else
    SetText(FTextItems[AValue]);
end;

procedure TPicklistSetting.Load(Ini: TCustomIniFile; const Section: string);
var
  s :string;
begin
  s := Ini.StringRead(Section, 'TextItems');
  FTextItems := s.Split(',');
  FDisplayItems := Ini.ReadLang(Section, 'DisplayItems', s, Settings.Language).Split(',');
  FMode := StrToMode(Ini.ReadString(Section, 'Mode', 'Text'));
  if FTextItems.Count<>FDisplayItems.Count then
    raise Exception.CreateFmt(SErrItemsCountMismatchFmt, [Section, FTextItems.Count]);
  inherited Load(Ini, Section);
end;

procedure TPicklistSetting.SetText(const AValue: string);
var
  i :integer;
begin
  if Mode=pmList then begin
    if FTextItems.TryFind(AValue, i, true) then begin
      FItemIndex := i;
      inherited SetText(FTextItems[i]);
    end else begin
      FItemIndex := -1;
      inherited SetText('');
    end;
  end else begin
    if FTextItems.TryFind(AValue, i, true) then begin
      FItemIndex := i;
      inherited SetText(FTextItems[i]);
    end else begin
      FItemIndex := -1;
      inherited SetText(AValue);
    end;
  end;
end;

function TPicklistSetting.GetText: string;
begin
  if Mode=pmList then begin
    if FItemIndex=-1 then
      result := ''
    else
      result := FTextItems[FItemIndex];
  end else begin
    if FItemIndex=-1 then
      result := inherited GetText
    else
      result := FTextItems[FItemIndex];
  end;
end;

function TPicklistSetting.GetDisplay: string;
begin
  if Mode=pmList then begin
    if FItemIndex=-1 then
      result := ''
    else
      result := FDisplayItems[FItemIndex];
  end else begin
    if FItemIndex=-1 then
      result := inherited GetDisplay
    else
      result := FDisplayItems[FItemIndex];
  end;
end;

procedure TPicklistSetting.SetDisplay(const AValue: string);
begin
  if Mode=pmList then begin
    if FTextItems.TryFind(AValue, FItemIndex, true) then begin
      SetText(FTextItems[FItemIndex]);
    end else begin
      SetText('');
    end;
  end else begin
    if FTextItems.TryFind(AValue, FItemIndex, true) then begin
      SetText(FTextItems[FItemIndex]);
    end else begin
      SetText(AValue);
    end;
  end;
end;

constructor TPicklistSetting.Create(Settings: TSettings; const Key: string);
begin
  inherited Create(Settings, Key);
  FMode := pmText;
  FItemIndex := -1;
end;

class function TPicklistSetting.StrToMode(const Str: string): TMode;
begin
  if SameText('List', Str) then
    result := pmList
  else if SameText('Text', Str) then
    result := pmText
  else
    raise Exception.CreateFmt(SErrInvalidPicklistModeFmt, [Str]);
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

procedure TIntegerSetting.SetText(const AValue: string);
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
  sa :TStringArray;
begin
  inherited Load(Ini, Section);
  sa := Ini.ReadLang(Section, 'Displays', FDisplays[False]+','+FDisplays[True], FSettings.FLanguage).Split(',');
  if sa.Count<>2 then
    raise Exception.CreateFmt(SErrItemsCountMismatchFmt, [Section, 2]);
  FDisplays[False] := sa[0];
  FDisplays[True] := sa[1];
end;

procedure TBooleanSetting.SetText(const AValue: string);
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

constructor TSettingsList.Create(const AGroup: string; OwnsObjects: boolean);
const
  OWNERSHIP :array[boolean] of TDictionaryOwnerships = ([], [doOwnsValues]);
begin
  inherited Create(OWNERSHIP[OwnsObjects]);
  FGroup := AGroup;
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
  SettingsSection :string;
  ClassId :string;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do
      if Section.StartsWith(Group+'.') then begin
        SettingsSection := Copy(Section, Length(Group)+2, Length(Section)-Length(Group)-1);
        ClassId := Ini.ReadString(Section, 'Class', '');
        if not TryGetValue(ClassId, Settings) then begin
          Settings := TSettings.Create(SettingsSection);
          Add(SettingsSection, Settings);
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
  TSetting.Register([TStringSetting, TBooleanSetting, TIntegerSetting, TPicklistSetting]);
end;

finalization
begin
  SettingClasses.Free;
  SettingsClasses.Free;
end;

end.



