unit Presentations;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, FileUtil,
  GalleryProcessor, Logging, StrUtils, StringArrays, Settings;

type
  TCustomManager = class;
  TCustomManagerClass = class of TCustomManager;
  TManagers = class;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;
  TFrameClass = class of TFrame;

  { TCustomManager }

  // A TCustomManager encapsulates a representaion of a PresentationManager
  TCustomManager = class
  private
    FId :string;              // From Section: i.e. "Slideshow200"
    FTitle :string;           // "Slideshow 2.0"
    FDescription :string;     // "Full-Screen Slideshow"
    FLongDescription :string;         // "Displays a single image and navigates through a list.
    FDate :TDateTime;
    FIconFile :string;
    FIcon :TPicture;
    FPreviewFile :string;
    FPreview :TPicture;
    FTemplateFolder :string;
    FTargetFolder :string;
    FTargetTitle :string;
    FFrame :TFrame;
    FSettings :TSettings;
    function GetIcon: TGraphic;
    function GetPreview: TGraphic;
    function GetFrame :TFrame;
  protected
    function GetFrameClass :TFrameClass; virtual;
    procedure ParamsToFrame; virtual;
    procedure FrameToParams; virtual;
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    destructor Destroy; override;
    class function ClassId :string;
    class function Create(const Filename :string) :TCustomManager; overload;
    class procedure Scan(const Folder :string; Managers :TManagers);
    //procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    //procedure LoadSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute; virtual; abstract;
    property Id :string read FId;
    property Frame :TFrame read GetFrame;
    property Settings :TSettings read FSettings write FSettings;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property LongDescription :string read FLongDescription;
    property Date :TDateTime read FDate;
    property Icon :TGraphic read GetIcon;
    property Preview :TGraphic read GetPreview;
    property TargetTitle :string read FTargetTitle write FTargetTitle;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
  end;

  { TManagers }

  TManagers = class(TObjectList<TCustomManager>)
  private
    FDictionary :TDictionary<string, TCustomManager>;
    function GetById(Id : string): TCustomManager;
  protected
    procedure Notify(constref AValue: TCustomManager; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Register(ManagerClass :TCustomManagerClass);
    procedure SortByDate;
    function IndexOf(const Id :string) :integer;
    property ById[Id :string] :TCustomManager read GetById;
  end;

  { TPresentationManager }

  TPresentationManager = class(TCustomManager)
  private
    FProcessor :TProcessor;
  protected
    function GetFrameClass :TFrameClass; override;
    procedure ParamsToFrame; override;
    procedure FrameToParams; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TColorPresentationManager }

  TColorPresentationManager = class(TPresentationManager)
  private
    FTitleColor :TColor;
    FButtonColor :TColor;
    FBackgroundColor :TColor;
  protected
    function GetFrameClass :TFrameClass; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
  end;

  { TPresentationManagerSettings }

  TPresentationManagerSettings = class(TSettings)
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

  { TSlideshow200Settings }

  TSlideshow200Settings = class(TPresentationManagerSettings)
  end;

  { TSimple100Settings }

  TSimple100Settings = class(TPresentationManagerSettings)
  end;


const
  PRESENTATIONS_FOLDER = 'presentations';
  PRESENTATIONSECTION = 'Presentation';

implementation

uses
  LazFileUtils, presentationmanagerfrm, colorpresentationmanagerfrm;

var
  ManagerClasses :TDictionary<string, TCustomManagerClass>;

const
  COMMON_SECTION    = 'Common';
  PRESESENTATION_INITYPE = 'PRD';
  PRESENTATIONFILE_EXTENSION = 'prd';

resourcestring
  SErrMissingPresentationManagerClassFmt = 'Missing PresentationManager class entry in ''%s''.';
  SErrUnregisterPresentationManagerFmt = 'Unregistered PresentationManager class ''%s''.';
  SErrIniValueNotFoundFmt = 'Key ''[%s]%s'' not found in ''%s''.';

{ TCustomManager }

constructor TCustomManager.Create(IniFile :TCustomIniFile);
const
  UNDEFINED = '<undefined>';
var
  Lang, FallbackLang :string;

  function IniRead(const Key :string; MustExist :boolean = false) :string;
  const
    DEFAULTS :array[boolean] of string = ('', UNDEFINED);
  begin
    result := IniFile.ReadString(PRESENTATIONSECTION, Format('%s.%s', [Key, FallbackLang]), UNDEFINED);
    if result = UNDEFINED then
      result := IniFile.ReadString(PRESENTATIONSECTION, Key, DEFAULTS[MustExist]);
    if MustExist and (result = UNDEFINED) then
      raise Exception.CreateFmt(SErrIniValueNotFoundFmt, [PRESENTATIONSECTION, Key, IniFile.Filename]);
  end;

begin
  inherited Create;
  GetLanguageIDs(Lang, FallBackLang);
  FTitle            := IniRead('Title');
  FId               := IniRead('Id', true);
  FDescription      := IniRead('Description');
  FLongDescription  := IniRead('LongDescription');
  FTemplateFolder   := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate             := IniFile.ReadDateTime(PRESENTATIONSECTION, 'Date', 0.0);
  FIconFile         := CreateAbsolutePath(IniFile.ReadString(PRESENTATIONSECTION, 'Icon', ''), FTemplateFolder);
  FPreviewFile      := CreateAbsolutePath(IniFile.ReadString(PRESENTATIONSECTION, 'Preview', ''), FTemplateFolder);
end;

destructor TCustomManager.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
  FFrame.Free;
  FSettings.Free;
  inherited Destroy;
end;

class function TCustomManager.ClassId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-8);
end;

function TCustomManager.GetIcon: TGraphic;
begin
  if not Assigned(FIcon) and (FIconFile<>'') then begin
    FIcon := TPicture.Create;
    FIcon.LoadFromFile(FIconFile);
  end;
  result := FIcon.Graphic;
end;

function TCustomManager.GetPreview: TGraphic;
begin
  if not Assigned(FPreview) then begin
    FPreview := TPicture.Create;
    if FPreviewFile<>'' then
      FPreview.LoadFromFile(FPreviewFile);
  end;
  result := FPreview.Graphic;
end;

function TCustomManager.GetFrameClass: TFrameClass;
begin
  result := nil;
end;

procedure TCustomManager.ParamsToFrame;
begin

end;

procedure TCustomManager.FrameToParams;
begin

end;

class function TCustomManager.Create(const Filename :string): TCustomManager;
var
  IniFile :TCustomIniFile;
  PresentationManagerClass :TCustomManagerClass;
  ClassId :string;
  ClassName :string;
begin
  IniFile := TIniFile.Create(Filename, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]);
  try
    ClassId := IniFile.ReadString(PRESENTATIONSECTION, 'Class', 'Presentation');
    ClassName := Format('T%sManager', [ClassId]);
    if not ManagerClasses.TryGetValue(ClassName, PresentationManagerClass) then
      raise Exception.CreateFmt(SErrUnregisterPresentationManagerFmt, [ClassName]);
    result := PresentationManagerClass.Create(IniFile);
  except
    IniFile.Free;
    raise;
  end;
end;

class procedure TCustomManager.Scan(const Folder: string; Managers :TManagers);
var
  WprFilenames :TStringList;
  Filename :string;
begin
  WprFilenames := FindAllFiles(Folder, '*.'+PRESENTATIONFILE_EXTENSION, true);
  try
    for Filename in WprFilenames do begin
      try
        Managers.Add(TCustomManager.Create(Filename));
      except on E :Exception do
        Log(E.Message, llWarning);
      end;
    end;
    Managers.SortByDate;
  finally
    WprFilenames.Free;
  end;
end;

function TCustomManager.GetFrame: TFrame;
var
  FrameClass :TFrameClass;
begin
  if not Assigned(FFrame) then begin
    FrameClass := GetFrameClass;
    if Assigned(FrameClass) then begin
      FFrame := GetFrameClass.Create(nil);
      FFrame.Name := Format('Frame%8.8x', [longint(FFrame)]);
      ParamsToFrame;
    end;
  end;
  result := FFrame;
end;

{ TManagers }

function TManagers.GetById(Id : string): TCustomManager;
begin
  if not FDictionary.TryGetValue(Id, result) then
    result := nil;
end;

procedure TManagers.Notify(constref AValue: TCustomManager; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FDictionary) then begin
    case ACollectionNotification of
    cnAdded:
      FDictionary.Add(AValue.Id, AValue);
    cnRemoved, cnExtracted:
      FDictionary.Remove(AValue.Id);
    end;
  end;
  inherited Notify(AValue, ACollectionNotification);
end;

constructor TManagers.Create;
begin
  inherited Create(false);
  FDictionary := TDictionary<string, TCustomManager>.Create;
end;

destructor TManagers.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

class procedure TManagers.Register(ManagerClass: TCustomManagerClass);
begin
  // Assumes TIdPresentationManager
  ManagerClasses.Add(ManagerClass.Classname, ManagerClass);
end;

function CompareDate(constref Left, Right :TCustomManager) :integer;
begin
  result := CompareDateTime(Left.FDate, Right.FDate);
end;

procedure TManagers.SortByDate;
begin
  Sort(TComparer<TCustomManager>.Construct(@CompareDate));
end;

function TManagers.IndexOf(const Id: string): integer;
var
  i :integer;
begin
  for i:=0 to Count-1 do
    if SameText(Id, self[i].Id) then
      Exit(i);
  result := -1;
end;

{ TPresentationManager }

function TPresentationManager.GetFrameClass: TFrameClass;
begin
  result := TPresentationManagerFrame;
end;

procedure TPresentationManager.ParamsToFrame;
begin
//var
//  s :TPresentationManagerSettings;
//begin
//  inherited ParamsToFrame;
//  if Assigned(FSettings) then begin
//    s := FSettings as TPresentationManagerSettings;
//    with Frame as TPresentationManagerFrame do begin
//      EditTitle.Text := s.Title; // FManager.DocumentVars['TITLE'];
//    end;
//  end;
end;

procedure TPresentationManager.FrameToParams;
begin
end;
//var
//  s :TPresentationManagerSettings;
//  inherited FrameToParams;
//  if Assigned(FSettings) then begin
//    s := FSettings as TPresentationManagerSettings;
//    with Frame as TPresentationManagerFrame do begin
//      s.Title := EditTitle.Text;
//    end;
//    FProcessor.DocumentVars['TITLE'] := s.Title;
//  end;

constructor TPresentationManager.Create(IniFile: TCustomIniFile);
var
  SectionKeys :TStringList;
  Key :string;
  VarName, VarValue :string;

  function MakeAbsoluteList(const Line :string) :TStringArray;
  var
    i :integer;
  begin
    if Line='' then Exit(nil);
    result := Line.Split(',');
    for i:=0 to result.Count-1 do
      result[i] := CreateAbsolutePath(result[i], FTemplateFolder);
  end;

begin
  inherited Create(IniFile);
  FProcessor := GalleryProcessor.TProcessor.Create;
  FProcessor.CopyFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATIONSECTION, 'Copy', ''));
  FProcessor.TemplateFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATIONSECTION, 'Templates', ''));
  FProcessor.DocumentVars.Add('TITLE');
  SectionKeys := TStringList.Create;
  try
    IniFile.ReadSection(PRESENTATIONSECTION, SectionKeys);
    for Key in SectionKeys do
      if StartsText('Fragment.', Key) then begin
        VarName := Copy(Key, 10, Length(Key)-9);
        VarValue := IniFile.ReadString(PRESENTATIONSECTION, Key, '');
        FProcessor.ListFragments.Add(VarName, VarValue);
      end;
  finally
    SectionKeys.Free;
  end;
end;

destructor TPresentationManager.Destroy;
begin
  FProcessor.Free;
  inherited Destroy;
end;

function TColorPresentationManager.GetFrameClass: TFrameClass;
begin
  result := TColorPresentationManagerFrame;
end;

procedure TPresentationManager.Execute;
var
  Stats :TProcessor.TStats;
begin
  FrameToParams;
  FProcessor.TargetFolder := TargetFolder;
  FProcessor.DocumentVars['TITLE'] := TargetTitle;
  FProcessor.Execute(Stats);
end;

{ TPresentationManagerSettings }

procedure TPresentationManagerSettings.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Changed;
end;

procedure TPresentationManagerSettings.Defaults;
begin
  FTitle := '';
end;

function TPresentationManagerSettings.Compare(const Value: TSettings): boolean;
var
  Settings :TPresentationManagerSettings;
begin
  Settings := Value as TPresentationManagerSettings;
  result := FTitle= Settings.Title;
end;

procedure TPresentationManagerSettings.Assign(const Value: TSettings);
var
  Settings :TPresentationManagerSettings;
begin
  Settings := Value as TPresentationManagerSettings;
  Title := Settings.Title;
end;

procedure TPresentationManagerSettings.SaveToIni(Ini: TCustomIniFile);
begin
  inherited;
  with Ini do begin
    WriteString(Section,'Title', Title);
  end;
end;

procedure TPresentationManagerSettings.LoadFromIni(Ini: TCustomIniFile);
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

{ TColorPresentationManager }

constructor TColorPresentationManager.Create(IniFile: TCustomIniFile);
begin
  inherited Create(IniFile);
  FProcessor.DocumentVars.Add('COLOR');
  FProcessor.DocumentVars.Add('SYMBOL-COLOR');
  FProcessor.DocumentVars.Add('BACKGROUND-COLOR');
end;

initialization
begin
  ManagerClasses := TDictionary<string, TCustomManagerClass>.Create;
  TManagers.Register(TPresentationManager);
  TManagers.Register(TColorPresentationManager);

//  TSettings.Register(TPresentationManagerSettings);
  TSettings.Register(TSimple100Settings);
  TSettings.Register(TSlideshow200Settings);
end;

finalization
begin
  ManagerClasses.Free;
end;

end.

