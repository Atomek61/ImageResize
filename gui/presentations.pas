unit Presentations;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, FileUtil,
  GalleryProcessor, Logging, StrUtils, StringArrays, Settings,
  PresentationManagerFrm, SettingsEditor;

type
  TCustomManager = class;
  TCustomManagerClass = class of TCustomManager;
  TManagers = class;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;
  TFrameClass = class of TFrame;

  { TCustomManager }

  // A TCustomManager encapsulates a representation of a PresentationManager
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
    function GetIcon: TGraphic;
    function GetPreview: TGraphic;
  protected
    procedure ParamsToFrame; virtual;
    procedure FrameToParams; virtual;
    function GetFrame :TFrame; virtual;
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    destructor Destroy; override;
    class procedure Register(ManagerClass :TCustomManagerClass);
    class function ClassId :string;
    class function Create(const Filename :string) :TCustomManager; overload;
    class procedure Scan(const Folder :string; Managers :TManagers);
    //procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    //procedure LoadSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute; virtual; abstract;
    property Id :string read FId;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property LongDescription :string read FLongDescription;
    property Date :TDateTime read FDate;
    property Icon :TGraphic read GetIcon;
    property Preview :TGraphic read GetPreview;
    property TargetTitle :string read FTargetTitle write FTargetTitle;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
    property Frame :TFrame read GetFrame;
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
    procedure SortByDate;
    function IndexOf(const Id :string) :integer;
    property ById[Id :string] :TCustomManager read GetById;
  end;

  { TPresentationManager }

  TPresentationManager = class(TCustomManager)
  private
    FProcessor :TProcessor;
    FSettings :TSettings;
    FManagerFrame :TPresentationManagerFrame;
    FSettingsEditor :TSettingsEditor; // Link between ValuesListEditor on FMangerFrame and FSettings;
  protected
    procedure ParamsToFrame; override;
    procedure FrameToParams; override;
    function GetFrame :TFrame; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
    destructor Destroy; override;
    procedure Execute; override;
  end;

const
  PRESENTATIONS_FOLDER  = 'presentations';
  PRESENTATION_SECTION  = 'Presentation';
  PRESENTATIONS_GROUP   = 'Presentation';

implementation

uses
  LazFileUtils;

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

class procedure TCustomManager.Register(ManagerClass: TCustomManagerClass);
begin
  // Assumes TIdPresentationManager
  ManagerClasses.Add(ManagerClass.Classname, ManagerClass);
end;

constructor TCustomManager.Create(IniFile :TCustomIniFile);
const
  UNDEFINED = '<undefined>';
var
  Lang, FallbackLang :string;

  function IniRead(const Key :string; MustExist :boolean = false) :string;
  const
    DEFAULTS :array[boolean] of string = ('', UNDEFINED);
  begin
    result := IniFile.ReadString(PRESENTATION_SECTION, Format('%s.%s', [Key, FallbackLang]), UNDEFINED);
    if result = UNDEFINED then
      result := IniFile.ReadString(PRESENTATION_SECTION, Key, DEFAULTS[MustExist]);
    if MustExist and (result = UNDEFINED) then
      raise Exception.CreateFmt(SErrIniValueNotFoundFmt, [PRESENTATION_SECTION, Key, IniFile.Filename]);
  end;

begin
  inherited Create;
  GetLanguageIDs(Lang, FallBackLang);
  FTitle            := IniRead('Title');
  FId               := IniRead('Id', true);
  FDescription      := IniRead('Description');
  FLongDescription  := IniRead('LongDescription');
  FTemplateFolder   := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate             := IniFile.ReadDateTime(PRESENTATION_SECTION, 'Date', 0.0);
  FIconFile         := CreateAbsolutePath(IniFile.ReadString(PRESENTATION_SECTION, 'Icon', ''), FTemplateFolder);
  FPreviewFile      := CreateAbsolutePath(IniFile.ReadString(PRESENTATION_SECTION, 'Preview', ''), FTemplateFolder);
end;

destructor TCustomManager.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
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

procedure TCustomManager.ParamsToFrame;
begin

end;

procedure TCustomManager.FrameToParams;
begin

end;

function TCustomManager.GetFrame: TFrame;
begin
  result := nil;
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
    ClassId := IniFile.ReadString(PRESENTATION_SECTION, 'Class', 'Presentation');
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
  inherited Create(true);
  FDictionary := TDictionary<string, TCustomManager>.Create;
end;

destructor TManagers.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
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

procedure TPresentationManager.ParamsToFrame;
begin
//var
//  s :TManagerSettings;
//begin
//  inherited ParamsToFrame;
//  if Assigned(FSettings) then begin
//    s := FSettings as TManagerSettings;
//    with Frame as TPresentationManagerFrame do begin
//      EditTitle.Text := s.Title; // FManager.DocumentVars['TITLE'];
//    end;
//  end;
end;

procedure TPresentationManager.FrameToParams;
begin
end;

function TPresentationManager.GetFrame: TFrame;
begin
  if not Assigned(FManagerFrame) then begin
    FManagerFrame := TPresentationManagerFrame.Create(nil);
    FSettingsEditor := TSettingsEditor.Create;
    FSettingsEditor.Bind(FSettings, FManagerFrame.ValueListEditor);
  end;
  result := FManagerFrame;
end;

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
  FProcessor.CopyFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATION_SECTION, 'Copy', ''));
  FProcessor.TemplateFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATION_SECTION, 'Templates', ''));
  FProcessor.DocumentVars.Add('TITLE');
  SectionKeys := TStringList.Create;
  try
    IniFile.ReadSection(PRESENTATION_SECTION, SectionKeys);
    for Key in SectionKeys do
      if StartsText('Fragment.', Key) then begin
        VarName := Copy(Key, 10, Length(Key)-9);
        VarValue := IniFile.ReadString(PRESENTATION_SECTION, Key, '');
        FProcessor.ListFragments.Add(VarName, VarValue);
      end;
  finally
    SectionKeys.Free;
  end;
  FSettings := TSettings.Create;
  FSettings.Load(IniFile, 'Settings');
end;

destructor TPresentationManager.Destroy;
begin
  FProcessor.Free;
  FSettings.Free;
  FManagerFrame.Free;
  FSettingsEditor.Free;
  inherited Destroy;
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

initialization
begin
  ManagerClasses := TDictionary<string, TCustomManagerClass>.Create;
  TCustomManager.Register(TPresentationManager);
end;

finalization
begin
  ManagerClasses.Free;
end;

end.

