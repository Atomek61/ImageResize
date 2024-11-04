unit Presentations;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, FileUtil, StringUtils,
  GalleryProcessor, Logging, StrUtils, StringArrays, Settings,
  PresentationManagerFrm, SettingsEditors, Templates, WebUtils, Language;

const
  SYSPRESENTATIONAPP      = 'ImageResize Presentation';
  SYSPRESENTATIONVENDOR   = 'www.atomek.de';
  SYSPRESENTATIONVERSION  = '250201';

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
    FId :string;                  // From Section: i.e. "Slideshow200"
    FTitle :string;               // "Slideshow 2.0"
    FDescription :string;         // "Full-Screen Slideshow"
    FLongDescription :string;     // "Displays a single image and navigates through a list.
    FDate :TDateTime;
    FIconFile :string;
    FIcon :TPicture;
    FTemplateFolder :string;
    FImgTagsFilename :string;
    FSettings :TSettings;
    FTypeDelimiters :TTypeDelimiters;
    FPrdDir :string;
    FRootDir :string;
    function GetIcon: TGraphic;
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
    property RootDir :string read FRootDir;
    property Id :string read FId;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property LongDescription :string read FLongDescription;
    property Date :TDateTime read FDate;
    property Icon :TGraphic read GetIcon;
    property ImgTagsFilename :string read FImgTagsFilename write FImgTagsFilename;
    property Settings :TSettings read FSettings;
    property Delimiters :TTypeDelimiters read FTypeDelimiters;
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
    FParamsEditor :TSettingsEditor; // Link between ValuesListEditor on FMangerFrame and FSettings;
    FWebResource :string; // Entry for the web browser
  public
    function ShowFrame(Parent :TWinControl) :TFrame; override;
    procedure StoreParams; override;
    procedure HideFrame; override;
    constructor Create(IniFile :TCustomIniFile); override;
    destructor Destroy; override;
    property WebResource :string read FWebResource;
    procedure Execute; override;
  end;

const
  PRESENTATIONS_FOLDER  = 'presentations';
  PRESENTATION_SECTION  = 'Presentation';
  PRESENTATIONS_GROUP   = 'Presentation';

implementation

uses
  LazFileUtils;

type
  TSettingPresentationFn = function(Setting :TSetting) :string;

var
  ManagerClasses :TDictionary<string, TCustomManagerClass>;
  SettingPresentationFns :TDictionary<string, TSettingPresentationFn>;

const
//  COMMON_SECTION    = 'Common';
//  PRESESENTATION_INITYPE = 'PRD';
  PRESENTATIONFILE_EXTENSION = 'prd';

resourcestring
  SMsgLoadingFmt = 'Loading ''%s''...';
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

  function IniRead(const Key :string; MustExist :boolean = false) :string;
  const
    DEFAULTS :array[boolean] of string = ('', UNDEFINED);
  begin
    result := IniFile.ReadString(PRESENTATION_SECTION, Format('%s.%s', [Key, TLanguage.Code]), UNDEFINED);
    if result = UNDEFINED then
      result := IniFile.ReadString(PRESENTATION_SECTION, Key, DEFAULTS[MustExist]);
    if MustExist and (result = UNDEFINED) then
      raise Exception.CreateFmt(SErrIniValueNotFoundFmt, [PRESENTATION_SECTION, Key, IniFile.Filename]);
    if result.StartsWith('@') then
      result := LoadStringFromFile(FPrdDir+Copy(result, 2, Length(result)-1));
  end;

var
  Keys :TStringlist;
  Key :string;
begin
  inherited Create;
  FTypeDelimiters       := TTypeDelimiters.Create;
  FPrdDir           := IncludeTrailingPathDelimiter(CreateAbsolutePath(ExtractFilePath(IniFile.Filename), ExtractFilePath(IniFile.Filename)));
  FRootDir          := IncludeTrailingPathDelimiter(CreateAbsolutePath(IniFile.ReadString(PRESENTATION_SECTION, 'RootDir', FPrdDir), FPrdDir));
  FTitle            := IniRead('Title');
  FId               := IniRead('Id', true);
  FDescription      := IniRead('Description');
  FLongDescription  := IniRead('LongDescription');
  FTemplateFolder   := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate             := IniFile.ReadDateTime(PRESENTATION_SECTION, 'Date', 0.0);
  FIconFile         := CreateAbsolutePath(IniFile.ReadString(PRESENTATION_SECTION, 'Icon', ''), FTemplateFolder);

  Keys := TStringlist.Create;
  try
    IniFile.ReadSection(PRESENTATION_SECTION, Keys);
    for Key in Keys do if Key.StartsWith('Delimiters.') then
      FTypeDelimiters.Add(LowerCase(Copy(Key, 12, Length(Key)-11)), TDelimiters.StrToDelimiters(IniRead(Key)));
  finally
    Keys.Free;
  end;

  FSettings := TSettings.Create(FId);
  FSettings.LoadDef(IniFile, 'Settings');
end;

destructor TCustomManager.Destroy;
begin
  FTypeDelimiters.Free;
  FIcon.Free;
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
      Log(SMsgLoadingFmt, [ExtractFilename(Filename)], llHint);
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
    FParamsEditor.Bind(Settings, FManagerFrame.ValueListEditor);
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
  FProcessor := GalleryProcessor.TProcessor.Create(Delimiters);
  FProcessor.CopyFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATION_SECTION, 'Copy', ''));
  FProcessor.TemplateFiles := MakeAbsoluteList(IniFile.ReadString(PRESENTATION_SECTION, 'Templates', ''));
  FProcessor.DocVars.Add('TITLE');
  FWebResource := IniFile.ReadString(PRESENTATION_SECTION, 'WebResource', '');
  if FWebResource = '' then begin
    if FProcessor.TemplateFiles.Count>0 then
      FWebResource := ExtractFileName(FProcessor.TemplateFiles[0]);
  end;
  SectionKeys := TStringList.Create;
  try
    IniFile.ReadSection(PRESENTATION_SECTION, SectionKeys);
    for Key in SectionKeys do
      if StartsText('Fragment.', Key) then begin
        VarName := Copy(Key, 10, Length(Key)-9);
        VarValue := IniFile.ReadString(PRESENTATION_SECTION, Key, '');
        if VarValue.StartsWith('@') then
          VarValue := LoadStringFromFile(FPrdDir+Copy(VarValue, 2, Length(VarValue)-1));
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
  Setting :TSetting;
  ValuePresentation :string;
//  SettingPresentationFn :TSettingPresentationFn;
begin
  FParamsEditor.Flush;
  FProcessor.ImgTagsFilename := ImgTagsFilename;
  FProcessor.DocVars.Load('PRESENTATIONID', Id);
  FProcessor.DocVars.Load('PRESENTATIONTITLE', Title);
  with FProcessor.SysVars do begin
    Load('PRESENTATIONAPP', SYSPRESENTATIONAPP);
    Load('PRESENTATIONVENDOR', SYSPRESENTATIONVENDOR);
    Load('PRESENTATIONVERSION', SYSPRESENTATIONVERSION);
  end;

  // Make Settings available to the Processor
  for Setting in Settings.Items do begin
    //if SettingPresentationFns.TryGetValue(Setting.PresentationHint, SettingPresentationFn) then
    //  ValuePresentation := SettingPresentationFn(Setting)
    //else
    ValuePresentation := Setting.AsDisplay;
    FProcessor.DocVars.Load(UpperCase(Setting.Key), ValuePresentation);
  end;

  FProcessor.Execute(Stats);
end;

function AsWebColor(Setting :TSetting) :string;
begin
  result := ColorToHTMLColor((Setting as TUInt32Setting).Value);
end;

initialization
begin
  ManagerClasses := TDictionary<string, TCustomManagerClass>.Create;
  TCustomManager.Register(TPresentationManager);

  SettingPresentationFns := TDictionary<string, TSettingPresentationFn>.Create;
  SettingPresentationFns.Add('WebColor', @AsWebColor);
end;

finalization
begin
  SettingPresentationFns.Free;
  ManagerClasses.Free;
end;

end.

