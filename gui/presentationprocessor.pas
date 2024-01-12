unit presentationprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, FileUtil,
  GalleryProcessor, Logging, StrUtils, StringArrays, Settings;

type
  TCustomProcessor = class;
  TCustomProcessorClass = class of TCustomProcessor;
  TProcessors = class;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;
  TFrameClass = class of TFrame;

  { TCustomProcessor }

  TCustomProcessor = class
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
    class function Create(const Filename :string) :TCustomProcessor; overload;
    class procedure Scan(const Folder :string; Processors :TProcessors);
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
    property TargetFolder :string read FTargetFolder write FTargetFolder;
  end;

  { TProcessors }

  TProcessors = class(TObjectList<TCustomProcessor>)
  private
    FDictionary :TDictionary<string, TCustomProcessor>;
    function GetById(Id : string): TCustomProcessor;
  protected
    procedure Notify(constref AValue: TCustomProcessor; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Register(ProcessorClass :TCustomProcessorClass);
    procedure SortByDate;
    function IndexOf(const Id :string) :integer;
    property ById[Id :string] :TCustomProcessor read GetById;
  end;

  { TPresentationProcessor }

  TPresentationProcessor = class(TCustomProcessor)
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

  { TColorPresentationProcessor }

  TColorPresentationProcessor = class(TPresentationProcessor)
  private
    FTitleColor :TColor;
    FButtonColor :TColor;
    FBackgroundColor :TColor;
  protected
    function GetFrameClass :TFrameClass; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
  end;

  { TPresentationProcessorSettings }

  TPresentationProcessorSettings = class(TSettings)
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

  TSlideshow200Settings = class(TPresentationProcessorSettings)
  end;

  { TSimple100Settings }

  TSimple100Settings = class(TPresentationProcessorSettings)
  end;


const
  PRESENTATIONS_FOLDER = 'presentations';
  PRESENTATIONSECTION = 'Presentation';

implementation

uses
  LazFileUtils, presentationprocessorfrm, colorpresentationprocessorfrm;

var
  ProcessorClasses :TDictionary<string, TCustomProcessorClass>;

const
  COMMON_SECTION    = 'Common';
  PRESESENTATION_INITYPE = 'PRD';
  PRESENTATIONFILE_EXTENSION = 'prd';

resourcestring
  SErrMissingPresentationProcessorClassFmt = 'Missing PresentationProcessor class entry in ''%s''.';
  SErrUnregisterPresentationProcessorFmt = 'Unregistered PresentationProcessor class ''%s''.';
  SErrIniValueNotFoundFmt = 'Key ''[%s]%s'' not found in ''%s''.';

{ TCustomProcessor }

constructor TCustomProcessor.Create(IniFile :TCustomIniFile);
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

destructor TCustomProcessor.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
  FFrame.Free;
  FSettings.Free;
  inherited Destroy;
end;

class function TCustomProcessor.ClassId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-9);
end;

function TCustomProcessor.GetIcon: TGraphic;
begin
  if not Assigned(FIcon) and (FIconFile<>'') then begin
    FIcon := TPicture.Create;
    FIcon.LoadFromFile(FIconFile);
  end;
  result := FIcon.Graphic;
end;

function TCustomProcessor.GetPreview: TGraphic;
begin
  if not Assigned(FPreview) then begin
    FPreview := TPicture.Create;
    if FPreviewFile<>'' then
      FPreview.LoadFromFile(FPreviewFile);
  end;
  result := FPreview.Graphic;
end;

function TCustomProcessor.GetFrameClass: TFrameClass;
begin
  result := nil;
end;

procedure TCustomProcessor.ParamsToFrame;
begin

end;

procedure TCustomProcessor.FrameToParams;
begin

end;

class function TCustomProcessor.Create(const Filename :string): TCustomProcessor;
var
  IniFile :TCustomIniFile;
  PresentationProcessorClass :TCustomProcessorClass;
  ClassId :string;
  ClassName :string;
begin
  IniFile := TIniFile.Create(Filename, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]);
  try
    ClassId := IniFile.ReadString(PRESENTATIONSECTION, 'Class', 'Presentation');
    ClassName := Format('T%sProcessor', [ClassId]);
    if not ProcessorClasses.TryGetValue(ClassName, PresentationProcessorClass) then
      raise Exception.CreateFmt(SErrUnregisterPresentationProcessorFmt, [ClassName]);
    result := PresentationProcessorClass.Create(IniFile);
  except
    IniFile.Free;
    raise;
  end;
end;

class procedure TCustomProcessor.Scan(const Folder: string; Processors :TProcessors);
var
  WprFilenames :TStringList;
  Filename :string;
begin
  WprFilenames := FindAllFiles(Folder, '*.'+PRESENTATIONFILE_EXTENSION, true);
  try
    for Filename in WprFilenames do begin
      try
        Processors.Add(TCustomProcessor.Create(Filename));
      except on E :Exception do
        Log(E.Message, llWarning);
      end;
    end;
    Processors.SortByDate;
  finally
    WprFilenames.Free;
  end;
end;

function TCustomProcessor.GetFrame: TFrame;
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

{ TProcessors }

function TProcessors.GetById(Id : string): TCustomProcessor;
begin
  if not FDictionary.TryGetValue(Id, result) then
    result := nil;
end;

procedure TProcessors.Notify(constref AValue: TCustomProcessor; ACollectionNotification: TCollectionNotification);
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

constructor TProcessors.Create;
begin
  inherited Create(false);
  FDictionary := TDictionary<string, TCustomProcessor>.Create;
end;

destructor TProcessors.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

class procedure TProcessors.Register(ProcessorClass: TCustomProcessorClass);
begin
  // Assumes TIdPresentationProcessor
  ProcessorClasses.Add(ProcessorClass.Classname, ProcessorClass);
end;

function CompareDate(constref Left, Right :TCustomProcessor) :integer;
begin
  result := CompareDateTime(Left.FDate, Right.FDate);
end;

procedure TProcessors.SortByDate;
begin
  Sort(TComparer<TCustomProcessor>.Construct(@CompareDate));
end;

function TProcessors.IndexOf(const Id: string): integer;
var
  i :integer;
begin
  for i:=0 to Count-1 do
    if SameText(Id, self[i].Id) then
      Exit(i);
  result := -1;
end;

{ TPresentationProcessor }

function TPresentationProcessor.GetFrameClass: TFrameClass;
begin
  result := TPresentationProcessorFrame;
end;

procedure TPresentationProcessor.ParamsToFrame;
var
  s :TPresentationProcessorSettings;
begin
  inherited ParamsToFrame;
  if Assigned(FSettings) then begin
    s := FSettings as TPresentationProcessorSettings;
    with Frame as TPresentationProcessorFrame do begin
      EditTitle.Text := s.Title; // FProcessor.DocumentVars['TITLE'];
    end;
  end;
end;

procedure TPresentationProcessor.FrameToParams;
var
  s :TPresentationProcessorSettings;
begin
  inherited FrameToParams;
  if Assigned(FSettings) then begin
    s := FSettings as TPresentationProcessorSettings;
    with Frame as TPresentationProcessorFrame do begin
      s.Title := EditTitle.Text;
    end;
    FProcessor.DocumentVars['TITLE'] := s.Title;
  end;
end;

constructor TPresentationProcessor.Create(IniFile: TCustomIniFile);
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

destructor TPresentationProcessor.Destroy;
begin
  FProcessor.Free;
  inherited Destroy;
end;

function TColorPresentationProcessor.GetFrameClass: TFrameClass;
begin
  result := TColorPresentationProcessorFrame;
end;

procedure TPresentationProcessor.Execute;
var
  Stats :TProcessor.TStats;
begin
  FrameToParams;
  FProcessor.TargetFolder := TargetFolder;
  FProcessor.Execute(Stats);
end;

{ TPresentationProcessorSettings }

procedure TPresentationProcessorSettings.SetTitle(AValue: string);
begin
  if FTitle=AValue then Exit;
  FTitle:=AValue;
  Changed;
end;

procedure TPresentationProcessorSettings.Defaults;
begin
  FTitle := '';
end;

function TPresentationProcessorSettings.Compare(const Value: TSettings): boolean;
var
  Settings :TPresentationProcessorSettings;
begin
  Settings := Value as TPresentationProcessorSettings;
  result := FTitle= Settings.Title;
end;

procedure TPresentationProcessorSettings.Assign(const Value: TSettings);
var
  Settings :TPresentationProcessorSettings;
begin
  Settings := Value as TPresentationProcessorSettings;
  Title := Settings.Title;
end;

procedure TPresentationProcessorSettings.SaveToIni(Ini: TCustomIniFile);
begin
  inherited;
  with Ini do begin
    WriteString(Section,'Title', Title);
  end;
end;

procedure TPresentationProcessorSettings.LoadFromIni(Ini: TCustomIniFile);
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

{ TColorPresentationProcessor }

constructor TColorPresentationProcessor.Create(IniFile: TCustomIniFile);
begin
  inherited Create(IniFile);
  FProcessor.DocumentVars.Add('COLOR');
  FProcessor.DocumentVars.Add('SYMBOL-COLOR');
  FProcessor.DocumentVars.Add('BACKGROUND-COLOR');
end;

initialization
begin
  ProcessorClasses := TDictionary<string, TCustomProcessorClass>.Create;
  TProcessors.Register(TPresentationProcessor);
  TProcessors.Register(TColorPresentationProcessor);

//  TSettings.Register(TPresentationProcessorSettings);
  TSettings.Register(TSimple100Settings);
  TSettings.Register(TSlideshow200Settings);
end;

finalization
begin
  ProcessorClasses.Free;
end;

end.

