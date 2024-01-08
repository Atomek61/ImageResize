unit webtasks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TTask = class;
  TTaskClass = class of TTask;
  TTasks = TObjectList<TTask>;

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

  { TReplaceTask }

  TReplaceTask = class(TTask)
  public
    constructor Create(WebProcessor :TWebProcessor); override;
  end;

implementation

var
  TaskClasses :TDictionary<string, TTaskClass>;

resourcestring
  SErrTaskClassNotRegisteredFmt = 'Unregistered WebProcessor Task class ''%s''.';
  SErrTaskClassNotFoundFmt      = 'In WebProcessor ''%s'' [%s] ''Class='' not found.';

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
    TaskClasses.Add(TaskClass.ClassName, TaskClass);
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

  { TReplaceTask }

  constructor TReplaceTask.Create(WebProcessor: TWebProcessor);
  begin
    inherited Create(WebProcessor);
  end;

initialization
begin
  TaskClasses := TDictionary<string, TTaskClass>.Create;
  TTask.Register(TCopyTask);
  TTask.Register(TReplaceTask);
end;

finalization
begin
  TaskClasses.Free;
end;


end.

