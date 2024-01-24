unit settings;

{$mode delphi}

// TSettings is alist of variables with Key, Caption, Text, Type, Presentation,
// Text and other attributes.
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
  Classes, SysUtils, IniFiles, Generics.Collections, Logging, IniFilesHelper,
  Translations, StringArrays;

type
  TCopyMode = (cmValues, cmWeak, cmStrong, cmDeep);
  TLoadMode = (lmEasygoing, lmStrict);

  TSetting = class;
  TSettingClass = class of TSetting;
  TSettingList = TObjectList<TSetting>;
  TSettingDictionary = TDictionary<string, TSetting>;

  TSettings = class;
  TSettingsClass = class of TSettings;
  TSettingsDictonary = TObjectDictionary<string, TSettings>;

  { TSettingsList }

  // A list of Section/Setting-Pairs
  TSettingsList = class(TSettingsDictonary)
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
    procedure SetDefaults;
    procedure LoadFromIni(Ini :TCustomIniFile; const Group :string = ''; Mode :TLoadMode = lmEasygoing);
    procedure SaveToIni(Ini :TCustomIniFile; const Group :string = '');
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property ChangedSettings :TSettings read FChangedSettings;
    property Dirty :boolean read FDirty;
  end;

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
//    procedure Clear; override;
    function IsEqual(const Other :TSettings) :boolean;
    procedure Copy(const Source :TSettings; Mode :TCopyMode);
    procedure Load(Ini :TCustomIniFile; const Section :string);
    procedure SaveToIni(Ini :TCustomIniFile; const Section :string = ''); virtual;
    procedure LoadFromIni(Ini :TCustomIniFile; const Section :string = ''; Mode :TLoadMode = lmStrict); virtual;
    property Items :TSettingList read FItems;
    property Section :string read FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property Dirty :boolean read FDirty write FDirty;
    property Language :string read FLanguage;
  end;

  { TSetting }

  TSetting = class
  private
    FCaption: string;
    FSettings :TSettings;
    FKey :string;
    FDefaultText :string;
    FPresentation :string; // Hint, how to display
    FOnChanged :TNotifyEvent;
  protected
    procedure Changed;
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    function GetDisplay :string; virtual;
    procedure SetDisplay(const AValue :string); virtual;
    procedure SetText(const AValue: string); virtual; abstract;
    function GetText :string; virtual; abstract;
    procedure DoClone(var Clone :TSetting); virtual;
  public
    class procedure Register(const Classes :array of TSettingClass);
    class function ClassDefault :string; virtual; abstract;
    class function ClassId :string;
    constructor Create(Settings :TSettings; const Key :string); virtual;
    function IsEqual(Other :TSetting) :boolean; virtual;
    procedure Assign(Source :TSetting); virtual;
    procedure SetDefault;
    function Clone(Settings :TSettings) :TSetting;
    property Settings :TSettings read FSettings;
    property Caption :string read FCaption;
    property Key :string read FKey;
    property Text :string read GetText write SetText;
    property Display :string read GetDisplay write SetDisplay;
    property DefaultText :string read FDefaultText write FDefaultText;
    property Presentation :string read FPresentation;
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
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
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
    procedure DoClone(var Clone :TSetting); override;
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
    procedure DoClone(var Clone :TSetting); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
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
    procedure DoClone(var Clone :TSetting); override;
  public
    constructor Create(Settings :TSettings; const Key :string); override;
    class function ClassDefault :string; override;
    function IsEqual(Other :TSetting) :boolean; override;
    procedure Assign(Source :TSetting); override;
    property Value :boolean read FValue write SetValue;
  end;

implementation

uses
  Math;

resourcestring
  SCptTrue = 'True';
  SCptFalse = 'False';
  SErrSettingClassNotFoundFmt = 'Setting class ''%s'' from [''%s''] not registered.';
  SErrConvertToIntegerFmt = 'Cant convert ''%s'' to integer.';
  SErrConvertToBooleanFmt = 'Cant convert ''%s'' to boolean.';
  SErrItemsCountMismatchFmt = 'Number of ''%s'' display strings mismatch, %d expected';
  SErrAssigningSectionFmt = 'Cant assign values of [%s] to those of [%s].';
  SErrAssignClassFmt = 'Cant assign %s value to %s value.';
  SErrAssignKeyFmt = 'Cant assign value of %s to %s.';
  SErrInvalidPicklistModeFmt = 'Invalid picklist mode ''%s''.';

var
  SettingClasses :TDictionary<string, TSettingClass>;
  SettingsClasses :TDictionary<string, TSettingsClass>;

const
  BOOLEANSTRINGS :array[boolean] of string = ('False', 'True');

function IfThen(Condition :boolean; const ThenStr, ElseStr :string) :string;
begin
  if Condition then
    result := ThenStr
  else
    result := ElseStr;
end;

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
    FSection := System.Copy(ClassName, 2, Length(ClassName)-9);
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

procedure TSettings.SetDefaults;
var
  Setting :TSetting;
begin
  for Setting in Items do
    Setting.SetDefault;
end;

//procedure TSettings.Clear;
//begin
//  inherited Clear;
//  FItems.Clear;
//  FDirty := False;
//end;

function TSettings.IsEqual(const Other: TSettings): boolean;
var
  i :integer;
begin
  result := Count = Other.Count;
  if result then
    for i:=0 to Items.Count-1 do
      if not Items[i].IsEqual(Other.Items[i]) then Exit(false);
end;

procedure TSettings.Copy(const Source: TSettings; Mode :TCopyMode);
var
  SourceSetting :TSetting;
  SelfSetting :TSetting;
  i :integer;
begin
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
  cmWeak:
    // Assign existing only
    for SourceSetting in Source.Items do begin
      if TryGetValue(SourceSetting.Key, SelfSetting) then
        SelfSetting.Text := SourceSetting.Text;
    end;
  cmStrong:
    // Assign existing, create missing
    for SourceSetting in Source.Items do begin
      if TryGetValue(SourceSetting.Key, SelfSetting) then
        SelfSetting.Assign(SourceSetting)
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
        SettingKey := System.Copy(SettingSection, Length(SectionDot)+1, Length(SettingSection)-Length(SectionDot));
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
  FDirty := False;
end;

function TSettings.Add(constref Setting: TSetting) :SizeInt;
begin
  inherited Add(Setting.Key, Setting);
  result := FItems.Add(Setting);
end;

procedure TSettings.SaveToIni(Ini: TCustomIniFile; const Section :string = '');
var
  Sect :string;
  Setting :TSetting;
begin
  if Section<>'' then
    Sect := Section
  else
    Sect := self.Section;
  for Setting in FItems do
    Ini.WriteString(Sect, Setting.Key, Setting.Text);
  FDirty := false;
end;

procedure TSettings.LoadFromIni(Ini: TCustomIniFile; const Section :string; Mode :TLoadMode);
var
  Setting :TSetting;
  Lines :TStrings;
  i :integer;
  Sctn :string;
begin
  Sctn := IfThen(Section='', self.Section, Section);
  case Mode of
  lmStrict:
    begin
      for Setting in FItems do
        Setting.Text := Ini.ReadString(Sctn, Setting.Key, Setting.DefaultText);
    end;
  lmEasygoing:
    begin
      Lines := TStringList.Create;
      try
        Ini.ReadSectionValues(Sctn, Lines);
        FItems.Clear;
        for i:=0 to Lines.Count-1 do begin
          TStringSetting.Create(self, Lines.Names[i]).Text := Lines.ValueFromIndex[i];
        end;
      finally
        Lines.Free;
      end;
    end;
  end;
  FDirty := false;
end;

constructor TSetting.Create(Settings: TSettings; const Key :string);
begin
  inherited Create;
  FKey := Key;
  FSettings := Settings;
  FDefaultText := ClassDefault;
  FSettings.Add(self);
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

procedure TSetting.SetDefault;
begin
  Text := FDefaultText;
end;

function TSetting.Clone(Settings :TSettings): TSetting;
begin
  result := TSettingClass(ClassType).Create(Settings, FKey);
  DoClone(result);
end;

procedure TSetting.Load(Ini: TCustomIniFile; const Section :string);
begin
//  FKey := LastItemAfterDot(Section);
  FCaption := Ini.ReadLang(Section, 'Caption', FSettings.Flanguage);
  FDefaultText := Ini.ReadString(Section, 'Default', ClassDefault);
  Text := Ini.ReadLang(Section, 'Text', '', FSettings.Flanguage);
  FPresentation := Ini.ReadString(Section, 'Presentation', ClassId);
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

procedure TSetting.DoClone(var Clone: TSetting);
begin
  Clone.FCaption := FCaption;
  Clone.FPresentation := FPresentation;
  Clone.FDefaultText := FDefaultText;
  Clone.Assign(self);
end;

class procedure TSetting.Register(const Classes :array of TSettingClass);
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

{ TStringSetting }

procedure TStringSetting.SetText(const AValue: string);
begin
  if AValue=Text then Exit;
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

function TStringSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (Value = TStringSetting(Other).Value);
end;

procedure TStringSetting.Assign(Source: TSetting);
begin
  inherited Assign(Source);
  if not IsEqual(Source) then begin
    FValue := TStringSetting(Source).FValue;
    Changed;
  end;
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
  Index :integer;
begin
  if FTextItems.TryFind(AValue, Index, true) then begin
    if FItemIndex=Index then Exit;
    FItemIndex := Index;
    Changed;
  end else begin
    if Mode=pmList then begin
      if FItemIndex=-1 then Exit;
    end else
      FValue := AValue;
    FItemIndex := -1;
    Changed;
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
var
  Index :integer;
begin
  if FDisplayItems.TryFind(AValue, Index, true) then begin
    if FItemIndex=Index then Exit;
    FItemIndex := Index;
    Changed;
  end else begin
    if Mode=pmList then begin
      if FItemIndex=-1 then Exit;
    end else
      FValue := AValue;
    FItemIndex := -1;
    Changed;
  end;
end;

procedure TPicklistSetting.DoClone(var Clone: TSetting);
begin
  inherited DoClone(Clone);
  with Clone as TPicklistSetting do begin
    FItemIndex := self.FItemIndex;
    FTextItems := System.Copy(self.FTextItems);
    FDisplayItems := System.Copy(self.FDisplayItems);
    FMode := self.FMode;
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

procedure TIntegerSetting.DoClone(var Clone: TSetting);
begin
  inherited DoClone(Clone);
  with Clone as TIntegerSetting do begin
    FMin := self.FMin;
    FMax := self.FMax;
  end;
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

function TIntegerSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (Value = TIntegerSetting(Other).Value);
end;

procedure TIntegerSetting.Assign(Source: TSetting);
begin
  inherited Assign(Source);
  if not IsEqual(Source) then begin
    FValue := TIntegerSetting(Source).FValue;
    Changed;
  end;
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

procedure TBooleanSetting.DoClone(var Clone: TSetting);
begin
  inherited DoClone(Clone);
  with Clone as TBooleanSetting do begin
    FDisplays[False] := self.FDisplays[False];
    FDisplays[True] := self.FDisplays[True];
  end;
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

function TBooleanSetting.IsEqual(Other: TSetting): boolean;
begin
  result := inherited IsEqual(Other) and (Value = TBooleanSetting(Other).Value);
end;

procedure TBooleanSetting.Assign(Source: TSetting);
begin
  inherited Assign(Source);
  if not IsEqual(Source) then begin
    FValue := TBooleanSetting(Source).FValue;
    Changed;
  end;
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

constructor TSettingsList.Create;
begin
  inherited Create([doOwnsValues]);
end;

procedure TSettingsList.SetDefaults;
var
  Settings :TSettings;
begin
  for Settings in self.Values do
    Settings.SetDefaults;
end;

procedure TSettingsList.LoadFromIni(Ini: TCustomIniFile; const Group :string; Mode :TLoadMode);
var
  Sections :TStrings;
  Section :string;
  Settings :TSettings;
  SettingsSection :string;
  ClassId :string;
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
        Settings.LoadFromIni(Ini, Prefix+SettingsSection, Mode);
      end;
    end;
  finally
    Sections.Free;
  end;
end;

procedure TSettingsList.SaveToIni(Ini: TCustomIniFile; const Group :string);
var
  Settings :TSettings;
begin
  for Settings in Values do
    Settings.SaveToIni(Ini, IfThen(Group='', Settings.Section, Group+'.'+Settings.Section));
end;

//procedure Test;
//var
//  s1 :TSettings;
//  s2 :TSettings;
//begin
//  s1 := TSettings.Create('Mother');
//  TStringSetting.Create(s1, 'Title');
//  TIntegerSetting.Create(s1, 'Weight');
//  s1['Title'].Text := 'My Mother';
//  s1['Weight'].Text := '120';
//
//  s2 := TSettings.Create('Mother');
//  s2.Copy(s1, cmStrong);
//end;
//
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



