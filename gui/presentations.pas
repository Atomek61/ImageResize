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
    FParams :TSettings;
    function GetIcon: TGraphic;
    function GetPreview: TGraphic;
  protected
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    destructor Destroy; override;
    class procedure Register(ManagerClass :TCustomManagerClass);
    class function ClassId :string;
    class function Create(const Filename :string) :TCustomManager; overload;
    class procedure Scan(const Folder :string; Managers :TManagers);
    function ShowFrame(Parent :TWinControl) :TFrame; virtual;
    procedure StoreParams; virtual; // Before Execution or before closing the dialog
    procedure HideFrame; virtual;
    procedure Execute; virtual;
    property Id :string read FId;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property LongDescription :string read FLongDescription;
    property Date :TDateTime read FDate;
    property Icon :TGraphic read GetIcon;
    property Preview :TGraphic read GetPreview;
    property TargetTitle :string read FTargetTitle write FTargetTitle;
    property TargetFolder :string read FTargetFolder write FTargetFolder;
    property Params :TSettings read FParams;
  end;

  { TManagers }

  TManagers = class(TObjectList<TCustomManager>)
  public
    procedure SortByDate;
    function TryFind(const Id :string; out Index :integer) :boolean;
  end;

  { TPresentationManager }

  TPresentationManager = class(TCustomManager)
  private
    FProcessor :TProcessor;
    FManagerFrame :TPresentationManagerFrame;
    FParamsEditor :TSettingsEditor; // Link between ValuesListEditor on FMangerFrame and FParams;
  protected
    function ShowFrame(Parent :TWinControl) :TFrame; override;
    procedure StoreParams; override;
    procedure HideFrame; override;
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

  FParams := TSettings.Create(FId);
  FParams.Load(IniFile, 'Settings');
end;

destructor TCustomManager.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
  FParams.Free;
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

function TCustomManager.ShowFrame(Parent :TWinControl): TFrame;
begin
  result := nil;
end;

procedure TCustomManager.StoreParams;
begin

end;

procedure TCustomManager.HideFrame;
begin
  StoreParams;
end;

procedure TCustomManager.Execute;
begin
  StoreParams;
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
  finally
    IniFile.Free;
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

function CompareDate(constref Left, Right :TCustomManager) :integer;
begin
  result := CompareDateTime(Left.FDate, Right.FDate);
end;

procedure TManagers.SortByDate;
begin
  Sort(TComparer<TCustomManager>.Construct(@CompareDate));
end;

function TManagers.TryFind(const Id: string; out Index :integer): boolean;
var
  i :integer;
begin
  for i:=0 to Count-1 do
    if SameText(self[i].Id, Id) then begin
      Index := i;
      Exit(true);
    end;
  result := false;
end;

{ TPresentationManager }

function TPresentationManager.ShowFrame(Parent :TWinControl) :TFrame;
begin
  if not Assigned(FManagerFrame) then begin
    FManagerFrame := TPresentationManagerFrame.Create(nil);
    FManagerFrame.Name := Format('ParamsFrame%8.8p', [@FManagerFrame]);
    FManagerFrame.Parent := Parent;
    FParamsEditor := TSettingsEditor.Create;
    FParamsEditor.Bind(Params, FManagerFrame.ValueListEditor);
  end;
  FManagerFrame.Visible := FParamsEditor.Editors.Count>0;
  result := FManagerFrame;
end;

procedure TPresentationManager.StoreParams;
begin
  if Assigned(FParamsEditor) then
    FParamsEditor.Flush;
end;

procedure TPresentationManager.HideFrame;
begin
  inherited;
  FManagerFrame.Visible := false;
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
end;

destructor TPresentationManager.Destroy;
begin
  FProcessor.Free;
  FManagerFrame.Free;
  FParamsEditor.Free;
  inherited Destroy;
end;

procedure TPresentationManager.Execute;
var
  Stats :TProcessor.TStats;
begin
  FParamsEditor.Flush;
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

