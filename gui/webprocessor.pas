unit webprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections,
  webprocessorinfofrm, taskparamsfrm, taskparamfrm;

type
  TWebProcessor = class;
  TWebProcessorClass = class of TWebProcessor;
  TWebProcessors = TObjectList<TWebProcessor>;
  TTask = class;
  TTaskClass = class of TTask;
  TTaskClasses = TDictionary<string, TTaskClass>;
  TTasks = TObjectList<TTask>;
  TFeature = (wpfDialog);
  TFeatures = set of TFeature;

  { TWebProcessor }

  TWebProcessor = class
  private
    FFolder :string;
    FFeatures :TFeatures;
    FCaption :string;
    FTitle :string;
    FDescription :string;
    FDate :TDateTime;
    FIniFile :TCustomIniFile;
    FIconFile :string;
    FTasks :TTasks;
    FTargetFolder :string;
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
    property Folder :string read FFolder;
    property Features :TFeatures read FFeatures;
    property Caption :string read FCaption;
    property Date :TDateTime read FDate;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property IconFile :string read FIconFile;
    property TargetFolder :string read FTargetFolder;
  end;

  { TTask }

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

{ TWebProcessor }

constructor TWebProcessor.Create(IniFile :TCustomIniFile);
var
  TaskIndex :integer;
  TaskSection :string;
  TaskClassName :string;
  TaskClass :TTaskClass;
begin
  inherited Create;
  FTasks := TTasks.Create;
  FFolder := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FIniFile := IniFile;
  FDate := FIniFile.ReadDateTime(PROCESSOR_SECTION, 'Date', 0.0);
  FCaption := FIniFile.ReadString(PROCESSOR_SECTION, 'Caption', '');
  FTitle := FIniFile.ReadString(PROCESSOR_SECTION, 'Title', '');
  FDescription := FIniFile.ReadString(PROCESSOR_SECTION, 'Description', '');
  FIconFile := FIniFile.ReadString(PROCESSOR_SECTION, 'Icon', '');
  FFeatures := [];

  TaskIndex := 1;
  while true do begin
    TaskSection := Format('Task.%d', [TaskIndex]);
    if not IniFile.SectionExists(TaskSection) then break;
    TaskClassName := IniFile.ReadString(TaskSection, 'Class', '<undefined>');
    if not TaskClasses.TryGetValue(TaskClassName, TaskClass) then
      raise Exception.CreateFmt(SErrTaskClassNotRegisteredFmt, [TaskClassName]);
    FTasks.Add(TaskClass.Create(self));
    inc(TaskIndex);
  end;

end;

destructor TWebProcessor.Destroy;
begin
  FTasks.Free;
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
begin
  result := TWebProcessors.Create;
  try
    WprFilenames := FindAllFiles(Folder, '*.'+WPREXT, true);
    try
      for Filename in WprFilenames do begin
        result.Add(TWebProcessor.Create(Filename));
      end;
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

