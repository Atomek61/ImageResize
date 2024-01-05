unit webprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  webprocessorinfofrm, taskparamsfrm, taskparamfrm, Graphics, GetText,
  DateUtils;

type
  TWebProcessor = class;
  TWebProcessorClass = class of TWebProcessor;
  TWebProcessors = class;
  TTask = class;
  TTaskClass = class of TTask;
  TTaskClasses = TDictionary<string, TTaskClass>;
  TTasks = TObjectList<TTask>;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;

  { TWebProcessor }

  TWebProcessor = class
  private
    FId :string;              // "slideshow200"
    FTitle :string;           // "Slideshow 2.0"
    FDescription :string;     // "Full-Screen Slideshow"
    FComment :string;         // "Displays a single image and navigates through a list.
    FDate :TDateTime;
    FFolder :string;
    FFeatures :TFeatures;
    FIniFile :TCustomIniFile;
    FIconFile :string;
    FIcon :TPicture;
    FTasks :TTasks;
    FTargetFolder :string;
    function GetIcon: TBitmap;
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    destructor Destroy; override;
    class procedure Register(ProcessorClass :TWebProcessorClass);
    class function Create(const Filename :string) :TWebProcessor; overload;
    class function Scan(const Folder :string) :TWebProcessors;
    procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute;
    function GetParamsFrame(Parent :TWinControl) :TTaskParamsFrame;
    function GetInfoFrameClass :TWebProcessorInfoFrameClass; virtual;
    property Id :string read FId;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property Comment :string read FComment;
    property Folder :string read FFolder;
    property Features :TFeatures read FFeatures;
    property Date :TDateTime read FDate;
    property IconFile :string read FIconFile;
    property Icon :TBitmap read GetIcon;
    property TargetFolder :string read FTargetFolder;
  end;

  { TWebProcessors }

  TWebProcessors = class(TObjectList<TWebProcessor>)
  private
    FDictionary :TDictionary<string, TWebProcessor>;
    function GetById(Id : string): TWebProcessor;
  protected
    procedure Notify(constref AValue: TWebProcessor; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SortByDate;
    property ById[Id :string] :TWebProcessor read GetById;
  end;

  TTask = class
  private
    FWebProcessor :TWebProcessor;
  protected
    function GetParamFrameClass :TParamFrameClass; virtual;
  public
    constructor Create(WebProcessor :TWebProcessor); virtual;
    class procedure Register(TaskClass :TTaskClass);
    function GetParamFrame(Parent :TWinControl) :TTaskParamFrame;
    procedure Execute;
  end;

  { TCopyTask }

  TCopyTask = class(TTask)
  public
    constructor Create(WebProcessor :TWebProcessor); override;
  end;

implementation

uses
  FileUtil;

var
  WebProcessorClasses :TDictionary<string, TWebProcessorClass>;
  TaskClasses :TDictionary<string, TTaskClass>;

const
  COMMON_SECTION    = 'Common';
  PROCESSOR_SECTION = 'Processor';
  WPREXT = 'wpr';

resourcestring
  SErrUnregisterWebProcessorFmt = 'Unregistered WebProcessor class ''%s''.';
  SErrTaskClassNotRegisteredFmt = 'Unregistered WebProcessor Task class ''%s''.';
  SErrTaskClassNotFoundFmt      = 'In WebProcessor ''%s'' [%s] ''Class='' not found.';

{ TWebProcessor }

function TWebProcessor.GetIcon: TBitmap;
begin
  if not Assigned(FIcon) and (FIconFile<>'') then begin
    FIcon := TPicture.Create;
    FIcon.LoadFromFile(FIconFile);
  end;
  result := FIcon.Bitmap;
end;

constructor TWebProcessor.Create(IniFile :TCustomIniFile);
var
  TaskIndex :integer;
  TaskSection :string;
  TaskClassName :string;
  TaskClass :TTaskClass;
  Lang, FallbackLang :string;

  function IniRead(const Key :string) :string;
  begin
    result := IniFile.ReadString(PROCESSOR_SECTION, Format('%s.%s', [Key, FallbackLang]), '');
    if result = '' then
      result := IniFile.ReadString(PROCESSOR_SECTION, Key, '');
  end;

begin
  inherited Create;
  FIniFile := IniFile;
  GetLanguageIDs(Lang, FallBackLang);
  FId := ChangeFileExt(ExtractFilename(IniFile.Filename), '');
  FTitle := IniRead('Title');
  FDescription := IniRead('Description');
  FComment := IniRead('Comment');
  FTasks := TTasks.Create;
  FFolder := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FDate := FIniFile.ReadDateTime(PROCESSOR_SECTION, 'Date', 0.0);
  FIconFile := FIniFile.ReadString(PROCESSOR_SECTION, 'Icon', '');
  FFeatures := [];

  TaskIndex := 1;
  while true do begin
    TaskSection := Format('Task.%d', [TaskIndex]);
    if not IniFile.SectionExists(TaskSection) then break;
    TaskClassName := IniFile.ReadString(TaskSection, 'Class', '');
    if TaskClassName='' then
      raise Exception.CreateFmt(SErrTaskClassNotFoundFmt, [FId, TaskSection]);
    if not TaskClasses.TryGetValue(TaskClassName, TaskClass) then
      raise Exception.CreateFmt(SErrTaskClassNotRegisteredFmt, [TaskClassName]);
    FTasks.Add(TaskClass.Create(self));
    inc(TaskIndex);
  end;

end;

destructor TWebProcessor.Destroy;
begin
  FTasks.Free;
  FIcon.Free;
  inherited Destroy;
end;

class procedure TWebProcessor.Register(ProcessorClass: TWebProcessorClass);
begin
  // Assumes TIdWebProcessor
  WebProcessorClasses.Add(Copy(ProcessorClass.Classname, 2, Length(ProcessorClass.Classname)-13), ProcessorClass);
end;

class function TWebProcessor.Create(const Filename :string): TWebProcessor;
var
  IniFile :TCustomIniFile;
  c :TWebProcessorClass;
  cn :string;
begin
  IniFile := TIniFile.Create(Filename, [ifoStripComments, ifoCaseSensitive, ifoStripQuotes]);
  try
    cn := IniFile.ReadString(PROCESSOR_SECTION, 'Class', '<undefined>');
    if not WebProcessorClasses.TryGetValue(cn, c) then
      raise Exception.CreateFmt(SErrUnregisterWebProcessorFmt, [cn]);
    result := c.Create(IniFile);
  except
    IniFile.Free;
    raise;
  end;
end;

class function TWebProcessor.Scan(const Folder: string): TWebProcessors;
var
  WprFilenames :TStringList;
  Filename :string;
  WebProcessor :TWebProcessor;
begin
  result := TWebProcessors.Create;
  try
    WprFilenames := FindAllFiles(Folder, '*.'+WPREXT, true);
    try
      for Filename in WprFilenames do begin
        WebProcessor := TWebProcessor.Create(Filename);
        result.Add(WebProcessor);
      end;
      result.SortByDate;
    finally
      WprFilenames.Free;
    end;
  except
    result.Free;
    raise;
  end;
end;

procedure TWebProcessor.Execute;
begin

end;

function TWebProcessor.GetParamsFrame(Parent :TWinControl): TTaskParamsFrame;
var
  Task :TTask;
  TaskParamFrame :TTaskParamFrame;
  i :integer;
begin
  result := TTaskParamsFrame.Create(nil);
  i := 1;
  for Task in FTasks do begin
    TaskParamFrame := Task.GetParamFrame(result);
    if Assigned(TaskParamFrame) then begin
      TaskParamFrame.Name := Format('TaskParamFrame%d', [i]);
      TaskParamFrame.Visible := true;
      inc(i);
    end;
  end;
end;

function TWebProcessor.GetInfoFrameClass: TWebProcessorInfoFrameClass;
begin
  result := TWebProcessorInfoFrame;
end;

{ TWebProcessors }

function TWebProcessors.GetById(Id : string): TWebProcessor;
begin
  result := FDictionary[Id];
end;

procedure TWebProcessors.Notify(constref AValue: TWebProcessor;
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

constructor TWebProcessors.Create;
begin
  inherited Create(false);
  FDictionary := TDictionary<string, TWebProcessor>.Create;
end;

destructor TWebProcessors.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

//function CompareDateTime(constref Left, Right :TDateTime) :integer;
//begin
//  result := CompareDateTime(Left
//end;
//
procedure TWebProcessors.SortByDate;
begin
  Sort(@CompareDateTime);
end;

{ TTask }

function TTask.GetParamFrameClass: TParamFrameClass;
begin
  result := nil;
end;

constructor TTask.Create(WebProcessor: TWebProcessor);
begin
  inherited Create;
  FWebProcessor := WebProcessor;
end;

class procedure TTask.Register(TaskClass: TTaskClass);
begin
  // Assumes TXxxxTask
  TaskClasses.Add(Copy(TaskClass.Classname, 2, Length(TaskClass.Classname)-5), TaskClass);
end;

function TTask.GetParamFrame(Parent: TWinControl): TTaskParamFrame;
var
  ParamFrameClass :TParamFrameClass;
begin
  ParamFrameClass := GetParamFrameClass;
  if Assigned(ParamFrameClass) then
    result := ParamFrameClass.Create(Parent)
  else
    result := nil;
end;

procedure TTask.Execute;
begin

end;

{ TCopyTask }

constructor TCopyTask.Create(WebProcessor: TWebProcessor);
begin
  inherited;
end;

initialization
begin
  WebProcessorClasses := TDictionary<string, TWebProcessorClass>.Create;
  TaskClasses := TTaskClasses.Create;
  TTask.Register(TCopyTask);
end;

finalization
begin
  WebProcessorClasses.Free;
  TaskClasses.Free;
end;

end.

