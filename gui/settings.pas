unit settings;

{$mode delphi}

// TSettings is alist of variables with Key, Caption, Text, Display, Value
//
// This is the Order:
//
// Level 1             SettingsList
//                       (Group)
//                    /           \
// Level 2        Settings     Settings
//               (Section)      (Section)
//                /     \        /     \
// Level 3   Setting Setting Setting Setting
//            (Key)   (Key)   (Key)   (Key)
//

interface

uses
  Classes, SysUtils, StrUtils, Generics.Collections, IniFiles, IniFilesHelper,
  StringArrays, Language;

type
  TCopyMode = (cmValues, cmExisting, cmAll, cmDeep);
  TLoadMode = (lmEasygoing, lmStrict);

  TSetting = class;
  TSettingClass = class of TSetting;
  TSettingList = TObjectList<TSetting>;
  TSettingDictionary = TDictionary<string, TSetting>;

  TSettings = class;
  TSettingsClass = class of TSettings;
  TSettingsDictionary = TObjectDictionary<string, TSettings>;

  { TSettingsList }

  // A list of Section/Setting-Pairs
  TSettingsList = class(TSettingsDictionary)
  private
    FDirty :boolean;
    FOnChanged :TNotifyEvent;
    FChangedSettings :TSettings; // Which TSettings has been changed when OnChanged
    procedure DoChanged;
    procedure OnSettingsChanged(Sender :TObject);
  protected
    procedure KeyNotify(constref AKey: string; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create; overload;
    procedure Clear; override;
    procedure SetDefaults;
    procedure Load(Ini :TCustomIniFile; const Group :string = ''; Mode :TLoadMode = lmEasygoing);
    procedure Save(Ini :TCustomIniFile; const Group :string = '');
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property ChangedSettings :TSettings read FChangedSettings;
    property Dirty :boolean read FDirty;
  end;

  { TSettings }

  TSettings = class(TSettingDictionary)
  private
    FSection :string; // The Ini-Section
    FLangCode :string; // de, en, ...
    FDirty :boolean;
    FOnChanged :TNotifyEvent;
    FItems :TSettingList;
    FChanging :boolean;
    FChanged :boolean;
  protected
    procedure Change;
    function Add(constref Setting :TSetting) :SizeInt;
    procedure SetDirty(Value :boolean);
  public
    class procedure Register(Value :TSettingsClass);
    constructor Create(const ASection :string = ''); virtual; overload;
    destructor Destroy; override;
    procedure SetDefaults; virtual;
    procedure LoadDef(Ini :TCustomIniFile; const ASection :string = '');
    function IsEqual(const Other :TSettings) :boolean;
    procedure Copy(const Source :TSettings; Mode :TCopyMode);
    procedure Save(Ini :TCustomIniFile; const Section :string = ''); virtual;
    procedure Load(Ini :TCustomIniFile; const Section :string = ''; Mode :TLoadMode = lmStrict); virtual;
    property Items :TSettingList read FItems;
    property Section :string read FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property Dirty :boolean read FDirty write SetDirty;
    property LangCode :string read FLangCode;
  end;

  { TSetting }

  // Value - inner Value of type Int32, String or whatever
  // AsText - persistent string representation
  // AsDisplay - language-depentent out presentation
  // Presentation - Hint how to Display

  TSetting = class
  private
    FSettings :TSettings;
    FKey :string;
    FCaption: string;
    FPresentationHint :string; // How to display
    FOnChanged :TNotifyEvent;
  protected
    procedure Change;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); virtual;
    procedure SetAsText(const AValue: string); virtual; abstract;
    function GetAsText :string; virtual; abstract;
    function GetAsDisplay :string; virtual;
    procedure SetAsDisplay(const AValue :string); virtual;
    procedure DoClone(var Clone :TSetting); virtual;
    procedure SetDefault; virtual; abstract;
  public
    class procedure Register(const Classes :array of TSettingClass);
    class function ClassDefault :string; virtual; abstract;
    class function ClassId :string;
    constructor Create(Settings :TSettings; const Key :string); virtual;
    function IsEqual(Other :TSetting) :boolean; virtual;
    procedure Assign(Source :TSetting); virtual;
    function Clone(Settings :TSettings) :TSetting;
    property Settings :TSettings read FSettings;
    property Key :string read FKey;
    property Caption :string read FCaption;
    property AsText :string read GetAsText write SetAsText;
    property AsDisplay :string read GetAsDisplay write SetAsDisplay;
    property PresentationHint :string read FPresentationHint;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TStringSetting }

  TStringSetting = class(TSetting)
  private
    FValue :string;
    FDefault :string;
  protected
    procedure SetAsText(const AValue: string); override;
    function GetAsText :string; override;
    procedure SetDefault; override;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
    property Value :string read FValue write SetAsText;
    property Default :string read FDefault write FDefault;
  end;

  { TInt32Setting }

  TInt32Setting = class(TSetting)
  private
    FValue :Int32;
    FDefault :Int32;
    procedure SetValue(AValue: Int32);
  protected
    procedure SetAsText(const AValue: string); override;
    function GetAsText :string; override;
    procedure SetDefault; override;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
    property Value :Int32 read FValue write SetValue;
    property Default :Int32 read FDefault write FDefault;
  end;

  { TExInt32Setting }

  TExInt32Setting = class(TInt32Setting)
  private
    FMin :Int32;
    FMax :Int32;
    procedure SetValue(AValue: Int32);
  protected
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    property Min :Int32 read FMin write FMin;
    property Max :Int32 read FMax write FMax;
  end;

  { TUInt32Setting }

  TUInt32Setting = class(TSetting)
  private
    FValue :UInt32;
    FDefault :UInt32;
    procedure SetValue(AValue: UInt32);
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
    property Value :UInt32 read FValue write SetValue;
    property Default :UInt32 read FDefault write FDefault;
  end;

  { TExUInt32Setting }

  TExUInt32Setting = class(TUInt32Setting)
  private
    FMin :UInt32;
    FMax :UInt32;
    procedure SetValue(AValue: UInt32);
  protected
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    property Min :UInt32 read FMin write FMin;
    property Max :UInt32 read FMax write FMax;
  end;

  { TBooleanSetting }

  TBooleanSetting = class(TSetting)
  private
    FValue :Boolean;
    FDefault :Boolean;
    procedure SetValue(AValue: Boolean);
  protected
    procedure SetAsText(const AValue: string); override;
    function GetAsText :string; override;
    procedure SetDefault; override;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
    property Value :Boolean read FValue write SetValue;
    property Default :Boolean read FDefault write FDefault;
  end;

  { TPickSetting }

  // Never instantiate
  TPickSetting = class(TStringSetting)
  protected
    FText :TStringArray;
    FDisplay :TStringArray;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); override;
  public
    property Text :TStringArray read FText;
    property Display :TStringArray read FDisplay;
  end;

  { TPicklistSetting }

  TPicklistSetting = class(TPickSetting)
  private
    FItemIndex :integer;
  protected
    procedure SetAsText(const AValue: string); override;
    function GetAsDisplay :string; override;
    procedure SetAsDisplay(const AValue: string); override;
    procedure SetItemIndex(AValue: integer);
  public
    property ItemIndex :integer read FItemIndex write SetItemIndex;
  end;

  { TPicktextSetting }

  TPicktextSetting = class(TPickSetting)
  protected
    function GetAsDisplay :string; override;
  end;

  function TryStrToBoolean(const Str :string; out Value :boolean) :boolean;
  function StrToBoolean(const Str :string) :boolean;
  function BooleanToStr(const Value :boolean) :string;

implementation

uses
  WebUtils, Graphics;

resourcestring
  SErrSettingClassNotFoundFmt = 'Setting class ''%s'' from [''%s''] not registered.';
  SErrConvertToInt32Fmt       = 'Cant convert ''%s'' to an Int32.';
  SErrConvertToUInt32Fmt      = 'Cant convert ''%s'' to an UInt32.';
  SErrConvertToBooleanFmt     = 'Cant convert ''%s'' to boolean.';
  SErrAssigningSectionFmt     = 'Cant assign values of [%s] to those of [%s].';
  SErrAssignClassFmt          = 'Cant assign %s value to %s value.';
  SErrAssignKeyFmt            = 'Cant assign value of %s to %s.';
  SErrInvalidPicklist         = 'Invalid picklist.';
  SErrInvalidPicklistDisplay  = 'Invalid picklist display.';

var
  SettingClasses :TDictionary<string, TSettingClass>;
  SettingsClasses :TDictionary<string, TSettingsClass>;

const
  BOOLEANSTRINGS :array[boolean] of string = ('False', 'True');

function RemoveQuotes(const Item :string) :string; overload;
var
  n :integer;
  s :string;
begin
  s := Trim(Item);
  n := Length(s);
  if (n<2) or (s[1]<>'"') or (s[n]<>'"') then Exit(s);
  result := Copy(s, 2, n-2);
end;

procedure RemoveQuotes(var Items :TStringArray); overload;
var i :integer;
begin
  for i:=0 to High(Items) do
    Items[i] := RemoveQuotes(Items[i]);
end;

function TryStrToInt32(const Str :string; out Value :longint) :boolean;
var
  s :string;
begin
  s := Trim(Str);
  if (Length(s)>0) and (s[1] = '#') then
    result := TryHTMLColorToColor(s, TColor(Value))
  else
    result := TryStrToInt(s, Value);
end;

function TryStrToBoolean(const Str: string; out Value: boolean): boolean;
begin
  // True   "True" n
  // False  "False" "0"
  if SameText(Str, BOOLEANSTRINGS[True]) or (Str='1') then
    Value := True
  else if SameText(Str, BOOLEANSTRINGS[False]) or (Str='0') then
    Value := False
  else
    Exit(False);
  result := True;
end;

function StrToBoolean(const Str: string): boolean;
begin
  if not TryStrToBoolean(Str, result) then
    raise EConvertError.CreateFmt(SErrConvertToBooleanFmt, [Str]);
end;

function BooleanToStr(const Value: boolean): string;
begin
  result := BOOLEANSTRINGS[Value];
end;

{ TSettings }

constructor TSettings.Create(const ASection: string);
begin
  inherited Create;
  if ASection<>'' then
    FSection := ASection
  else
    FSection := System.Copy(ClassName, 2, Length(ClassName)-9);
  FLangCode := TLanguage.Code; //GetLanguageID.LanguageCode;
  FItems := TSettingList.Create(true);
  SetDefaults;
  FChanged := False;
  FChanging := False;
end;

destructor TSettings.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSettings.Change;
begin
  FChanged := true;
  if not FChanging then begin
    FChanging := True;
    try
      Dirty := true;
      if Assigned(FOnChanged) then
        FOnChanged(self);
    finally
      FChanging := False;
    end;
  end;
end;

function TSettings.Add(constref Setting: TSetting): SizeInt;
begin
  inherited Add(Setting.Key, Setting);
  result := FItems.Add(Setting);
end;

procedure TSettings.SetDirty(Value: boolean);
begin
  if Dirty=Value then Exit;
  FDirty := Value;
  Change;
end;

class procedure TSettings.Register(Value: TSettingsClass);
begin
  SettingsClasses.Add(Value.ClassName, Value);
end;

procedure TSettings.SetDefaults;
var
  Setting :TSetting;
begin
  if not FChanging then begin
    FChanging := true;
    try
      FChanged := False;
      for Setting in Items do
        Setting.SetDefault;
      Dirty := false;
    finally
      FChanging := false;
    end;
    if FChanged and Assigned(FOnChanged) then
      FOnChanged(self);
  end;
end;

function TSettings.IsEqual(const Other: TSettings): boolean;
var
  i :integer;
begin
  result := Count = Other.Count;
  if result then
    for i:=0 to Items.Count-1 do
      if not Items[i].IsEqual(Other.Items[i]) then Exit(false);
end;

procedure TSettings.Copy(const Source: TSettings; Mode: TCopyMode);
var
  SourceSetting :TSetting;
  Setting :TSetting;
  i :integer;
begin
  if not FChanging then begin
    FChanging := true;
    try
      FChanged := False;
      if (Section<>Source.Section) then
        raise Exception.CreateFmt(SErrAssigningSectionFmt, [Source.Section, Section]);
      case Mode of
      cmValues:
        begin
          // Assign values, requires 1:1 relation
          if (Section<>Source.Section) or (Items.Count<>Source.Items.Count) then
            raise Exception.CreateFmt(SErrAssigningSectionFmt, [Source.Section, Section]);
          for i:=0 to Items.Count-1 do
            Items[i].Assign(Source.Items[i]);
        end;
      cmExisting:
        // Assign existing only
        for SourceSetting in Source.Items do begin
          if TryGetValue(SourceSetting.Key, Setting) then
            Setting.AsText := SourceSetting.AsText;
        end;
      cmAll:
        // Assign existing, create missing
        for SourceSetting in Source.Items do begin
          if TryGetValue(SourceSetting.Key, Setting) then
            Setting.Assign(SourceSetting)
          else
            SourceSetting.Clone(self);
        end;
      cmDeep:
        begin
          Clear;
          for SourceSetting in Source.Items do
            SourceSetting.Clone(self);
          Dirty := Source.Dirty;
        end;
      end;
    finally
      FChanging := false;
    end;
    if FChanged and Assigned(FOnChanged) then
      FOnChanged(self);
  end;
end;

procedure TSettings.LoadDef(Ini: TCustomIniFile; const ASection: string);
var
  Section :string;
  Sections :TStringList;
  SectionDot :string;
  SettingSection :string;
  Setting :TSetting;
  SettingKey :string;
  SettingClassName :string;
  SettingClass :TSettingClass;
begin
  Section := IfThen(ASection='', self.Section, ASection);
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    SectionDot := Section+'.';
    for SettingSection in Sections do
      if SettingSection.StartsWith(SectionDot, true) then begin
        SettingKey := System.Copy(SettingSection, Length(SectionDot)+1, Length(SettingSection)-Length(SectionDot));
        SettingClassName := 'T'+Ini.ReadString(SettingSection, 'Class', '')+'Setting';
        if not SettingClasses.TryGetValue(SettingClassName, SettingClass) then
          raise Exception.CreateFmt(SErrSettingClassNotFoundFmt, [SettingClassName, SettingSection]);
        Setting := SettingClass.Create(self, SettingKey);
        Setting.LoadDef(Ini, SettingSection);
      end;
  finally
    Sections.Free;
  end;
  FDirty := False;
end;

procedure TSettings.Save(Ini: TCustomIniFile; const Section: string);
var
  Sect :string;
  Setting :TSetting;
begin
  if not FChanging then begin
    FChanging := true;
    try
      FChanged := False;
      if Section<>'' then
        Sect := Section
      else
        Sect := self.Section;
      for Setting in FItems do
        Ini.WriteString(Sect, Setting.Key, Setting.AsText);
      Dirty := false;
    finally
      FChanging := false;
    end;
    if FChanged and Assigned(FOnChanged) then
      FOnChanged(self);
  end;
end;

procedure TSettings.Load(Ini: TCustomIniFile; const Section: string; Mode: TLoadMode);
var
  Setting :TSetting;
  Lines :TStrings;
  i :integer;
  Sctn :string;
begin
  if not FChanging then begin
    FChanging := true;
    try
      FChanged := False;
      Sctn := IfThen(Section='', self.Section, Section);
      case Mode of
      lmStrict:
        begin
          for Setting in FItems do
            Setting.AsText := Ini.ReadString(Sctn, Setting.Key, Setting.ClassDefault);
        end;
      lmEasygoing:
        begin
          Lines := TStringList.Create;
          try
            Ini.ReadSectionValues(Sctn, Lines);
            FItems.Clear;
            for i:=0 to Lines.Count-1 do begin
              TStringSetting.Create(self, Lines.Names[i]).AsText := Lines.ValueFromIndex[i];
            end;
          finally
            Lines.Free;
          end;
        end;
      end;
      Dirty := false;
    finally
      FChanging := false;
    end;
    if FChanged and Assigned(FOnChanged) then
      FOnChanged(self);
  end;
end;

{ TSettingsList }

constructor TSettingsList.Create;
begin
  inherited Create([doOwnsValues]);
end;

procedure TSettingsList.Clear;
begin
  inherited Clear;
  FDirty := false;
end;

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

procedure TSettingsList.KeyNotify(constref AKey: string;
  ACollectionNotification: TCollectionNotification);
begin
  if ACollectionNotification=cnAdded then
    self[AKey].OnChanged := self.OnSettingsChanged;
  inherited KeyNotify(AKey, ACollectionNotification);
end;

procedure TSettingsList.SetDefaults;
var
  Settings :TSettings;
begin
  for Settings in self.Values do
    Settings.SetDefaults;
  FDirty := false;
end;

procedure TSettingsList.Load(Ini: TCustomIniFile; const Group: string; Mode: TLoadMode);
var
  Sections :TStrings;
  Section :string;
  Settings :TSettings;
  SettingsSection :string;
  Prefix :string;
begin
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do begin
      if Group<>'' then Prefix := Group+'.' else Prefix := '';
      if Section.StartsWith(Prefix) then begin
        SettingsSection := Copy(Section, Length(Prefix)+1, Length(Section)-Length(Prefix));
        if not TryGetValue(SettingsSection, Settings) then begin
          Settings := TSettings.Create(SettingsSection);
          Add(SettingsSection, Settings);
        end;
        Settings.Load(Ini, Prefix+SettingsSection, Mode);
      end;
    end;
    FDirty := false;
  finally
    Sections.Free;
  end;
end;

procedure TSettingsList.Save(Ini: TCustomIniFile; const Group: string);
var
  Settings :TSettings;
begin
  for Settings in Values do
    Settings.Save(Ini, IfThen(Group='', Settings.Section, Group+'.'+Settings.Section));
  FDirty := false;
end;

{ TSetting }

constructor TSetting.Create(Settings: TSettings; const Key: string);
begin
  inherited Create;
  FKey := Key;
  FSettings := Settings;
  FSettings.Add(self);
end;

procedure TSetting.Change;
begin
  if Assigned(FOnChanged) then
    FOnChanged(self);
  FSettings.Change;
end;

procedure TSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  FCaption := Ini.ReadLang(Section, 'Caption', FSettings.FLangCode);
  FPresentationHint := Ini.ReadString(Section, 'Presentation', ClassId);
end;

function TSetting.GetAsDisplay: string;
begin
  result := AsText;
end;

procedure TSetting.SetAsDisplay(const AValue: string);
begin
  AsText := AValue;
end;

procedure TSetting.DoClone(var Clone: TSetting);
begin
  Clone.FCaption := FCaption;
  Clone.FPresentationHint := FPresentationHint;
  Clone.Assign(self);
end;

class procedure TSetting.Register(const Classes: array of TSettingClass);
var
  AClass :TSettingClass;
begin
  for AClass in Classes do
    SettingClasses.Add(AClass.ClassName, AClass);
end;

class function TSetting.ClassId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-8);
end;

function TSetting.IsEqual(Other: TSetting): boolean;
begin
  result := self.ClassType = Other.ClassType;
end;

procedure TSetting.Assign(Source: TSetting);
begin
  if ClassType <> Source.ClassType then
    raise Exception.CreateFmt(SErrAssignClassFmt, [Source.ClassName, ClassName]);
  if Key<>Source.Key then
    raise Exception.CreateFmt(SErrAssignKeyFmt, [Source.Key, Key]);
end;

function TSetting.Clone(Settings: TSettings): TSetting;
begin
  result := TSettingClass(ClassType).Create(Settings, FKey);
  DoClone(result);
end;

{ TStringSetting }

procedure TStringSetting.SetAsText(const AValue: string);
begin
  if AValue=Value then Exit;
  FValue := AValue;
  Change;
end;

function TStringSetting.GetAsText: string;
begin
  result := FValue;
end;

procedure TStringSetting.SetDefault;
begin
  Value := FDefault;
end;

procedure TStringSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FDefault := Ini.ReadLang(Section, 'Default', ClassDefault, Settings.LangCode);
  FValue := FDefault;
end;

class function TStringSetting.ClassDefault: string;
begin
  result := '';
end;

function TStringSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (FValue = TStringSetting(Other).FValue);
end;

procedure TStringSetting.Assign(Source: TSetting);
begin
  inherited;
  if not IsEqual(Source) then begin
    FValue := TStringSetting(Source).FValue;
    Change;
  end;
end;

{ TInt32Setting }

procedure TInt32Setting.SetValue(AValue: Int32);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  Change;
end;

procedure TInt32Setting.SetAsText(const AValue: string);
var
  Value :Int32;
begin
  if not TryStrToInt32(AValue, Value) then
    raise EConvertError.CreateFmt(SErrConvertToInt32Fmt, [AValue]);
  self.Value := Value;
end;

function TInt32Setting.GetAsText: string;
begin
  result := IntToStr(FValue);
end;

procedure TInt32Setting.SetDefault;
begin
  Value := FDefault;
end;

procedure TInt32Setting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FDefault := Ini.ReadInteger(Section, 'Default', 0);
  FValue := FDefault;
end;

constructor TInt32Setting.Create(Settings: TSettings; const Key: string);
begin
  inherited;
  FValue := 0;
  FDefault := 0;
end;

class function TInt32Setting.ClassDefault: string;
begin
  result := '0';
end;

function TInt32Setting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (FValue = TInt32Setting(Other).FValue);
end;

procedure TInt32Setting.Assign(Source: TSetting);
begin
  inherited;
  if not IsEqual(Source) then begin
    FValue := TInt32Setting(Source).FValue;
    Change;
  end;
end;

{ TExInt32Setting }

constructor TExInt32Setting.Create(Settings: TSettings; const Key: string);
begin
  inherited;
  FMin := Low(Int32);
  FMax := High(Int32);
end;

procedure TExInt32Setting.SetValue(AValue: Int32);
begin
  if AValue<FMin then AValue := FMin;
  if AValue>FMax then AValue := FMax;
  inherited
end;

procedure TExInt32Setting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FMin := Ini.ReadInteger(Section, 'Min', Low(Int32));
  FMax := Ini.ReadInteger(Section, 'Max', High(Int32));
end;

{ TUInt32Setting }

procedure TUInt32Setting.SetValue(AValue: UInt32);
begin
  if FValue=AValue then Exit;
  FValue:=AValue;
  Change;
end;

procedure TUInt32Setting.SetAsText(const AValue: string);
var
  Value :Int64;
begin
  if not (TryStrToInt64(AValue, Value) or (Value<Low(UInt32)) or (Value>High(UInt32))) then
    raise EConvertError.CreateFmt(SErrConvertToUInt32Fmt, [AValue]);
  self.Value := Value;
end;

function TUInt32Setting.GetAsText: string;
begin
  result := IntToStr(FValue);
end;

procedure TUInt32Setting.SetDefault;
begin
  Value := FDefault;
end;

procedure TUInt32Setting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FDefault := Ini.ReadInt64(Section, 'Default', 0);
  FValue := FDefault;
end;

constructor TUInt32Setting.Create(Settings: TSettings; const Key: string);
begin
  inherited;
  FValue := 0;
  FDefault := 0;
end;

class function TUInt32Setting.ClassDefault: string;
begin
  result := '0';
end;

function TUInt32Setting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (FValue = TUInt32Setting(Other).FValue);
end;

procedure TUInt32Setting.Assign(Source: TSetting);
begin
  inherited;
  if not IsEqual(Source) then begin
    FValue := TUInt32Setting(Source).FValue;
    Change;
  end;
end;

{ TExUInt32Setting }

constructor TExUInt32Setting.Create(Settings: TSettings; const Key: string);
begin
  inherited;
  FMin := Low(UInt32);
  FMax := High(UInt32);
end;

procedure TExUInt32Setting.SetValue(AValue: UInt32);
begin
  if AValue<FMin then AValue := FMin;
  if AValue>FMax then AValue := FMax;
  inherited
end;

procedure TExUInt32Setting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FMin := Ini.ReadInt64(Section, 'Min', Low(UInt32));
  FMax := Ini.ReadInt64(Section, 'Max', High(UInt32));
end;

{ TBooleanSetting }

procedure TBooleanSetting.SetValue(AValue: Boolean);
begin
  if FValue=AValue then Exit;
  FValue := AValue;
  Change;
end;

procedure TBooleanSetting.SetAsText(const AValue: string);
begin
  Value := StrToBoolean(AValue)
end;

function TBooleanSetting.GetAsText: string;
begin
  result := BooleanToStr(FValue);
end;

procedure TBooleanSetting.SetDefault;
begin
  Value := FDefault;
end;

procedure TBooleanSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited LoadDef(Ini, Section);
  FDefault := StrToBoolean(Ini.ReadString(Section, 'Default', ClassDefault));
  FValue := FDefault;
end;

class function TBooleanSetting.ClassDefault: string;
begin
  result := BOOLEANSTRINGS[False];
end;

function TBooleanSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (FValue = TBooleanSetting(Other).FValue);
end;

procedure TBooleanSetting.Assign(Source: TSetting);
begin
  inherited;
  if not IsEqual(Source) then begin
    FValue := TBooleanSetting(Source).FValue;
    Change;
  end;
end;

{ TPickSetting }

procedure TPickSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin
  inherited;
  FText := Ini.ReadString(Section, 'Text', '').Split(',');
  if FText.Count=0 then
    raise Exception.Create(SErrInvalidPicklist);
  RemoveQuotes(FText);
  FDisplay := Ini.ReadLang(Section, 'Display', '', FSettings.LangCode).Split(',');
  if FDisplay.Count<>FText.Count then
    raise Exception.Create(SErrInvalidPicklistDisplay);
  RemoveQuotes(FDisplay);
end;

{ TPicklistSetting }

procedure TPicklistSetting.SetAsText(const AValue: string);
var
  i :integer;
begin
  for i:=0 to High(FText) do
    if SameText(AValue, FText[i]) then begin
      ItemIndex := i;
      Exit;
    end;
end;

function TPicklistSetting.GetAsDisplay: string;
begin
  result := FDisplay[FItemIndex];
end;

procedure TPicklistSetting.SetAsDisplay(const AValue: string);
var
  i :integer;
begin
  for i:=0 to High(FDisplay) do
    if SameText(AValue, FDisplay[i]) then begin
      ItemIndex := i;
      Exit;
    end;
end;

procedure TPicklistSetting.SetItemIndex(AValue: integer);
begin
  if AValue<0 then AValue := 0 else if AValue>=FText.Count then AValue := FText.Count-1;
  if FItemIndex=AValue then Exit;
  FItemIndex:=AValue;
  FValue := FText[FItemIndex];
  Change;
end;

{ TPicktextSetting }

function TPicktextSetting.GetAsDisplay: string;
var
  i :integer;
begin
  result := inherited GetAsDisplay;
  for i:=0 to High(FText) do
    if SameText(result, FText[i]) then begin
      Exit(FDisplay[i]);
    end;
end;

{ TPicktextSetting }

//procedure TPicktextSetting.SetAsDisplay(const AValue: string);
//var
//  i :integer;
//begin
//  result := ;
//    for i:=0 to High(FText) do
//      if SameText(result, FText[i]) then begin
//        Exit(FDisplay[i]);
//      end;
//  end;
//  inherited SetAsDisplay(AValue);
//end;
//
//procedure Test;
//var
//  sa :TStringArray;
//const
//  STR :string = '123,"ab,c", xyz';
//begin
//  sa := STR.split([','], '"', '"');
//  RemoveQuotes(sa);
//end;
//
initialization
begin
//  Test;
  SettingsClasses := TDictionary<string, TSettingsClass>.Create;
  SettingClasses := TDictionary<string, TSettingClass>.Create;
  TSetting.Register([TStringSetting, TInt32Setting, TExInt32Setting, TUInt32Setting, TExUInt32Setting, TBooleanSetting, TPicklistSetting, TPicktextSetting]);
end;

finalization
begin
  SettingClasses.Free;
  SettingsClasses.Free;
end;

end.

