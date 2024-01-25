unit settings2;

{$mode delphi}

interface

uses
  Classes, SysUtils, Generics.Collections, IniFiles;

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

  { TSettings }

  TSettings = class(TSettingDictionary)
  private
    FSection :string; // The Ini-Section
    FLangCode :string; // de, en, ...
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
    procedure LoadDef(Ini :TCustomIniFile; const Section :string);
    procedure Save(Ini :TCustomIniFile; const Section :string = ''); virtual;
    procedure Load(Ini :TCustomIniFile; const Section :string = ''; Mode :TLoadMode = lmStrict); virtual;
    property Items :TSettingList read FItems;
    property Section :string read FSection;
    property OnChanged :TNotifyEvent read FOnChanged write FOnChanged;
    property Dirty :boolean read FDirty write FDirty;
    property LangCode :string read FLangCode;
  end;

  { TSetting }

  TSetting = class
  private
    FSettings :TSettings;
    FCaption: string;
    FKey :string;
    FDefaultText :string;
    FPresentationHint :string; // How to display
    FOnChanged :TNotifyEvent;
  protected
    procedure Changed;
    procedure LoadDef(Ini :TCustomIniFile; const Section :string); virtual;
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
    property PresentationHint :string read FPresentationHint;
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



implementation

uses
  Translations;

resourcestring
  SCptTrue = 'True';
  SCptFalse = 'False';
  SErrSettingClassNotFoundFmt = 'Setting class ''%s'' from [''%s''] not registered.';
  SErrConvertToIntegerFmt     = 'Cant convert ''%s'' to integer.';
  SErrConvertToBooleanFmt     = 'Cant convert ''%s'' to boolean.';
  SErrItemsCountMismatchFmt   = 'Number of ''%s'' display strings mismatch, %d expected';
  SErrAssigningSectionFmt     = 'Cant assign values of [%s] to those of [%s].';
  SErrAssignClassFmt          = 'Cant assign %s value to %s value.';
  SErrAssignKeyFmt            = 'Cant assign value of %s to %s.';
  SErrInvalidPicklistModeFmt  = 'Invalid picklist mode ''%s''.';

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

{ TSettings }

constructor TSettings.Create(const ASection: string);
begin
  inherited Create;
  if ASection<>'' then
    FSection := ASection
  else
    FSection := System.Copy(ClassName, 2, Length(ClassName)-9);
  FLangCode := GetLanguageID.LanguageCode;
  FItems := TSettingList.Create(true);
  SetDefaults;
end;

destructor TSettings.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TSettings.Changed;
begin
  FDirty := true;
  if Assigned(FOnChanged) then
    FOnChanged(self);
end;

function TSettings.Add(constref Setting: TSetting): SizeInt;
begin

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

procedure TSettings.LoadDef(Ini: TCustomIniFile; const Section: string);
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
          Setting.LoadDef(Ini, SettingSection);
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

procedure TSettings.Save(Ini: TCustomIniFile; const Section: string);
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

procedure TSettings.Load(Ini: TCustomIniFile; const Section: string; Mode: TLoadMode);
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

{ TSetting }

constructor TSetting.Create(Settings: TSettings; const Key: string);
begin

end;

procedure TSetting.Changed;
begin

end;

procedure TSetting.LoadDef(Ini: TCustomIniFile; const Section: string);
begin

end;

function TSetting.GetDisplay: string;
begin

end;

procedure TSetting.SetDisplay(const AValue: string);
begin

end;

procedure TSetting.DoClone(var Clone: TSetting);
begin

end;

class procedure TSetting.Register(const Classes: array of TSettingClass);
begin

end;

class function TSetting.ClassId: string;
begin

end;

function TSetting.IsEqual(Other: TSetting): boolean;
begin

end;

procedure TSetting.Assign(Source: TSetting);
begin

end;

procedure TSetting.SetDefault;
begin

end;

function TSetting.Clone(Settings: TSettings): TSetting;
begin

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

initialization
begin
  SettingsClasses := TDictionary<string, TSettingsClass>.Create;
  SettingClasses := TDictionary<string, TSettingClass>.Create;
  TSetting.Register([TStringSetting]);
//  TSetting.Register([TStringSetting, TBooleanSetting, TIntegerSetting, TPicklistSetting]);
end;

finalization
begin
  SettingClasses.Free;
  SettingsClasses.Free;
end;

end.

