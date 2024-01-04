unit webprocessor;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, Forms, IniFiles, Generics.Collections, webprocessorinfofrm;

type
  TWebProcessor = class;
  TWebProcessorClass = class of TWebProcessor;
  TTaskClass = class of TTask;
  TWebProcessors = TObjectList<TWebProcessor>;

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
  public
    constructor Create(IniFile :TCustomIniFile); virtual; overload;
    class procedure Register(ProcessorClass :TWebProcessorClass);
    class function Create(const Filename :string) :TWebProcessor; overload;
    class function Scan(const Folder :string) :TWebProcessors;
    procedure SaveSettings(const Section :string; Store :TCustomInifile); virtual; abstract;
    procedure Execute;
    function ShowDialog(Parent :TWinControl) :TFrame;
    function GetInfoFrameClass :TWebProcessorInfoFrameClass; virtual;
    property Folder :string read FFolder;
    property Features :TFeatures read FFeatures;
    property Caption :string read FCaption;
    property Date :TDateTime read FDate;
    property Title :string read FTitle;
    property Description :string read FDescription;
    property IconFile :string read FIconFile;
  end;

  { TTask }

  TTask = class
    constructor Create(WebProcessor :TWebProcessor);
    class procedure Register(TaskClass :TTaskClass);
    procedure Execute(const Section :string);
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

{ TWebProcessor }

constructor TWebProcessor.Create(IniFile :TCustomIniFile);
begin
  inherited Create;
  FFolder := IncludeTrailingPathDelimiter(ExtractFilePath(IniFile.Filename));
  FIniFile := IniFile;
  FDate := FIniFile.ReadDateTime(PROCESSOR_SECTION, 'Date', 0.0);
  FCaption := FIniFile.ReadString(PROCESSOR_SECTION, 'Caption', '');
  FTitle := FIniFile.ReadString(PROCESSOR_SECTION, 'Title', '');
  FDescription := FIniFile.ReadString(PROCESSOR_SECTION, 'Description', '');
  FIconFile := FIniFile.ReadString(PROCESSOR_SECTION, 'Icon', '');
  FFeatures := [];
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

function TWebProcessor.ShowDialog(Parent: TWinControl): TFrame;
begin
  result := nil;
end;

function TWebProcessor.GetInfoFrameClass: TWebProcessorInfoFrameClass;
begin
  result := TWebProcessorInfoFrame;
end;

{ TTask }

constructor TTask.Create(WebProcessor: TWebProcessor);
begin

end;

class procedure TTask.Register(TaskClass: TTaskClass);
begin
  // Assumes TIdTask
  TaskClasses.Add(Copy(TaskClass.Classname, 2, Length(TaskClass.Classname)-4), TaskClass);
end;

procedure TTask.Execute(const Section: string);
begin

end;

initialization
begin
  WebProcessorClasses := TDictionary<string, TWebProcessorClass>.Create;
  TaskClasses := TDictionary<string, TTaskClass>.Create;
end;

finalization
begin
  WebProcessorClasses.Free;
  TaskClasses.Free;
end;

end.

