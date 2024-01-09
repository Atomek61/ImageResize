unit webprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  Graphics, GetText, DateUtils, Generics.Defaults, FileUtil,
  GalleryProcessor, Logging, StrUtils, StringArrays;

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
    FId :string;              // "slideshow200"
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
    class function Scan(const Folder :string) :TProcessors;
    procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure LoadSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute; virtual; abstract;
    property Frame :TFrame read GetFrame;
    property Id :string read FId;
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
    property ById[Id :string] :TCustomProcessor read GetById;
  end;

  { TWebProcessor }

  TWebProcessor = class(TCustomProcessor)
  private
    FProcessor :TProcessor;
    FTitle :string;
  protected
    function GetFrameClass :TFrameClass; override;
    procedure ParamsToFrame; override;
    procedure FrameToParams; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TColorWebProcessor }

  TColorWebProcessor = class(TWebProcessor)
  private
    FTitleColor :TColor;
    FButtonColor :TColor;
    FBackgroundColor :TColor;
  protected
    function GetFrameClass :TFrameClass; override;
  public
    constructor Create(IniFile :TCustomIniFile); override;
  end;

implementation

uses
  LazFileUtils, WebProcessorFrm, ColorWebProcessorFrm;

var
  ProcessorClasses :TDictionary<string, TCustomProcessorClass>;

const
  COMMON_SECTION    = 'Common';
  PROCESSOR_SECTION = 'Processor';
  WPREXT = 'wpr';

resourcestring
  SErrMissingWebProcessorClassFmt = 'Missing WebProcessor class entry in ''%s''.';
  SErrUnregisterWebProcessorFmt = 'Unregistered WebProcessor class ''%s''.';

{ TCustomProcessor }

constructor TCustomProcessor.Create(IniFile :TCustomIniFile);
var
  Lang, FallbackLang :string;

  function IniRead(const Key :string) :string;
  begin
    result := IniFile.ReadString(PROCESSOR_SECTION, Format('%s.%s', [Key, FallbackLang]), '');
    if result = '' then
      result := IniFile.ReadString(PROCESSOR_SECTION, Key, '');
  end;

begin
  inherited Create;
  GetLanguageIDs(Lang, FallBackLang);
  FId               := ChangeFileExt(ExtractFilename(IniFile.Filename), '');
  FTitle            := IniRead('Title');
  FDescription      := IniRead('Description');
  FLongDescription  := IniRead('LongDescription');
  FTemplateFolder   := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate             := IniFile.ReadDateTime(PROCESSOR_SECTION, 'Date', 0.0);
  FIconFile         := CreateAbsolutePath(IniFile.ReadString(PROCESSOR_SECTION, 'Icon', ''), FTemplateFolder);
  FPreviewFile      := CreateAbsolutePath(IniFile.ReadString(PROCESSOR_SECTION, 'Preview', ''), FTemplateFolder);
end;

destructor TCustomProcessor.Destroy;
begin
  FIcon.Free;
  FPreview.Free;
  FFrame.Free;
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
  WebProcessorClass :TCustomProcessorClass;
  ClassId :string;
  ClassName :string;
begin
  IniFile := TIniFile.Create(Filename, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]);
  try
    ClassId := IniFile.ReadString(PROCESSOR_SECTION, 'Class', 'Web');
    //if ClassId='<undefined>' then
    //  raise Exception.CreateFmt(SErrMissingWebProcessorClassFmt, [Filename]);
    ClassName := Format('T%sProcessor', [ClassId]);
    if not ProcessorClasses.TryGetValue(ClassName, WebProcessorClass) then
      raise Exception.CreateFmt(SErrUnregisterWebProcessorFmt, [ClassName]);
    result := WebProcessorClass.Create(IniFile);
  except
    IniFile.Free;
    raise;
  end;
end;

class function TCustomProcessor.Scan(const Folder: string): TProcessors;
var
  WprFilenames :TStringList;
  Filename :string;
  WebProcessor :TCustomProcessor;
begin
  result := TProcessors.Create;
  WprFilenames := FindAllFiles(Folder, '*.'+WPREXT, true);
  try
    for Filename in WprFilenames do begin
      try
        WebProcessor := TCustomProcessor.Create(Filename);
        result.Add(WebProcessor);
      except on E :Exception do
        Log(E.Message, llWarning);
      end;
    end;
    result.SortByDate;
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
    end;
  end;
  result := FFrame;
end;

{ TProcessors }

function TProcessors.GetById(Id : string): TCustomProcessor;
begin
  result := FDictionary[Id];
end;

procedure TProcessors.Notify(constref AValue: TCustomProcessor;
  ACollectionNotification: TCollectionNotification);
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
  // Assumes TIdWebProcessor
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

{ TWebProcessor }

function TWebProcessor.GetFrameClass: TFrameClass;
begin
  result := TWebProcessorFrame;
end;

procedure TWebProcessor.ParamsToFrame;
begin
  inherited ParamsToFrame;
  with Frame as TWebProcessorFrame do begin
    EditTitle.Text := FProcessor.DocumentVars['TITLE'];
  end;
end;

procedure TWebProcessor.FrameToParams;
begin
  inherited FrameToParams;
  with Frame as TWebProcessorFrame do begin
    FProcessor.DocumentVars['TITLE'] := EditTitle.Text;
  end;
end;

constructor TWebProcessor.Create(IniFile: TCustomIniFile);
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
  FProcessor.CopyFiles := MakeAbsoluteList(IniFile.ReadString(PROCESSOR_SECTION, 'Copy', ''));
  FProcessor.TemplateFiles := MakeAbsoluteList(IniFile.ReadString(PROCESSOR_SECTION, 'Templates', ''));
  FProcessor.DocumentVars.Add('TITLE');
  SectionKeys := TStringList.Create;
  try
    IniFile.ReadSection(PROCESSOR_SECTION, SectionKeys);
    for Key in SectionKeys do
      if StartsText('Fragment.', Key) then begin
        VarName := Copy(Key, 10, Length(Key)-9);
        VarValue := IniFile.ReadString(PROCESSOR_SECTION, Key, '');
        FProcessor.ListFragments.Add(VarName, VarValue);
      end;
  finally
    SectionKeys.Free;
  end;
end;

destructor TWebProcessor.Destroy;
begin
  FProcessor.Free;
  inherited Destroy;
end;

function TColorWebProcessor.GetFrameClass: TFrameClass;
begin
  result := TColorWebProcessorFrame;
end;

procedure TWebProcessor.Execute;
var
  Stats :TProcessor.TStats;
begin
  FrameToParams;
  FProcessor.TargetFolder := TargetFolder;
  FProcessor.Execute(Stats);
end;

{ TColorWebProcessor }

constructor TColorWebProcessor.Create(IniFile: TCustomIniFile);
begin
  inherited Create(IniFile);
  FProcessor.DocumentVars.Add('COLOR');
  FProcessor.DocumentVars.Add('SYMBOL-COLOR');
  FProcessor.DocumentVars.Add('BACKGROUND-COLOR');
end;

initialization
begin
  ProcessorClasses := TDictionary<string, TCustomProcessorClass>.Create;
  TProcessors.Register(TWebProcessor);
  TProcessors.Register(TColorWebProcessor);
end;

finalization
begin
  ProcessorClasses.Free;
end;

end.

