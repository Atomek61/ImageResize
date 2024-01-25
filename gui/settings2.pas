unit settings2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics;

type
  TSetting = class;
  TSettingClass = class of TSetting;
  TSettingList = TObjectList<TSetting>;
  TSettingDictionary = TDictionary<string, TSetting>;

  TSettings = class;
  TSettingsClass = class of TSettings;
  TSettingsDictonary = TObjectDictionary<string, TSettings>;

  TSettings = class(TSettingDictionary)
  private
    FSection :string; // The Ini-Section
    FLangCode :string; // DE, EN
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



implementation

end.

