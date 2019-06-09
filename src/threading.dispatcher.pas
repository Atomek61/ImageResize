unit threading.dispatcher;

{$mode DELPHI}{$H+}

interface

uses
  Classes, SysUtils, generics.collections, generics.queue, SyncObjs;

type

  TDispatcher = class;
  TContext = class;
  TWorker = class;

  TLevel = (mlHint, mlNormal, mlWarning, mlAbort, mlFatal);

  TPrintEvent = procedure(Sender :TObject; WorkerId: integer; const Line :string; Level :TLevel) of object;
  TProgressEvent = procedure(Sender :TObject; Progress :single) of object;

  // The user must derive a task class and adds intances to a task list

  { TCustomTask }

  TCustomTask = class abstract
  private
    FWorker :TWorker;
  protected
    function Execute(Context :TContext) :boolean; virtual; abstract;
    property Worker :TWorker read FWorker;
    function GetTaskSteps :integer; virtual; // Progress system: tells dispatcher how many steps this task goes
    procedure Print(const Text :string; Level :TLevel = mlNormal);
    procedure Progress(Steps :integer);
  public
    property TaskSteps :integer read GetTaskSteps;
  end;

  { TTasks }

  TTasks = class(TObjectList<TCustomTask>)
  public
    function GetTotalSteps :integer;
  end;

  { TWorker }

  TWorker = class(TThread)
  private
    FContext :TContext;
    FTask :TCustomTask;
    FId :integer;
    procedure Start(Task :TCustomTask);
  protected
    procedure Execute; override;
  public
    constructor Create(Context :TContext; Id :integer);
    property Context :TContext read FContext;
    property Task :TCustomTask read FTask;
    property Id :integer read FId;
  end;

  { TMessage }

  TMessage = class
    Task :TCustomTask;
    constructor Create(Task :TCustomTask);
  end;

  { TPrintMessage }

  TPrintMessage = class(TMessage)
    Text :string;
    Level :TLevel;
    constructor Create(Sender :TCustomTask; const Value :string; Level :TLevel = mlNormal);
  end;

  { TProgressMessage }

  TProgressMessage = class(TMessage)
    Steps :integer;
    constructor Create(Sender :TCustomTask; Steps :integer);
  end;

  { TExitMessage }

  TExitMessage = class(TPrintMessage);

  { TMessages }

  TMessages = class(TQueue<TMessage>)
  private
    FQueueSection :TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    function GetCount :integer; override;
    procedure Push(Item :T); override;
    function Pop(out Item :T) :boolean; override;
  end;

  { TWorkers }

  TWorkers = class(TQueue<TWorker>)
  protected
    function Drop :boolean; override;
  public

  end;

  { TContext }

  TContext = class
  private
    FDispatcher :TDispatcher;
    FMessages :TMessages;
    function GetAborted: boolean;
  public
    constructor Create(Dispatcher :TDispatcher);
    destructor Destroy; override;
    property Aborted :boolean read GetAborted; // To be checked frequently
    property Messages :TMessages read FMessages;
  end;

  { TDispatcher }

  TDispatcher = class
  private
    FAborted :boolean;
    FMaxWorkerCount :integer;
    FStopOnError :boolean;
    FStepCount :integer;
    FTotalSteps :integer;
    FOnPrint :TPrintEvent;
    FOnProgress :TProgressEvent;
    procedure Print(Sender :TCustomTask; const Text :string; Level :TLevel);
    procedure Progress(Sender :TCustomTask; Steps :integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Execute(Tasks :TTasks) :boolean;
    procedure Abort;
    property MaxWorkerCount :integer read FMaxWorkerCount write FMaxWorkerCount;
    property StopOnError :boolean read FStopOnError write FStopOnError;
    property OnPrint :TPrintEvent read FOnPrint write FOnPrint;
    property OnProgress :TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

{ TCustomTask }

function TCustomTask.GetTaskSteps: integer;
begin
  result := 1;
end;

procedure TCustomTask.Print(const Text: string; Level: TLevel);
begin
  self.Worker.Context.Messages.Push(
    TPrintMessage.Create(self, Text, Level)
  );
end;

procedure TCustomTask.Progress(Steps: integer);
begin
  self.Worker.Context.Messages.Push(
    TProgressMessage.Create(self, Steps)
  );
end;

{ TTasks }

function TTasks.GetTotalSteps: integer;
var
  i :integer;
begin
  result := 0;
  for i:=0 to Count-1 do
    result += Items[i].GetTaskSteps;
end;

{ TProgressMessage }

constructor TProgressMessage.Create(Sender: TCustomTask; Steps :integer);
begin
  self.Steps := Steps;
  inherited Create(Sender);
end;

{ TPrintMessage }

constructor TPrintMessage.Create(Sender: TCustomTask; const Value: string; Level: TLevel);
begin
  self.Text := Value;
  self.Level := Level;
  inherited Create(Sender);
end;

{ TMessage }

constructor TMessage.Create(Task :TCustomTask);
begin
  inherited Create;
  self.Task := Task;
end;

{ TMessages }

constructor TMessages.Create;
begin
  inherited Create;
  FQueueSection := TCriticalSection.Create;
end;

destructor TMessages.Destroy;
begin
  inherited Destroy;
  FQueueSection.Free;
end;

function TMessages.GetCount: integer;
begin
  FQueueSection.Enter;
  result := inherited GetCount;
  FQueueSection.Leave;
end;

procedure TMessages.Push(Item: T);
begin
  FQueueSection.Enter;
  try
    inherited Push(Item);
  finally
    FQueueSection.Leave;
  end;
end;

function TMessages.Pop(out Item: T): boolean;
begin
  FQueueSection.Enter;
  try
    Result:=inherited Pop(Item);
  finally
    FQueueSection.Leave;
  end;
end;

{ TWorker }

constructor TWorker.Create(Context :TContext; Id :integer);
begin
  inherited Create(true);
  FContext := Context;
  FId := Id;
end;

procedure TWorker.Start(Task: TCustomTask);
begin
  FTask := Task;
  FTask.FWorker := self;
  Suspended := false;
end;

procedure TWorker.Execute;
var
  ExitMessage :TMessage;
begin
  while not Terminated do begin
    try
      if FTask.Execute(FContext) then
        ExitMessage := TExitMessage.Create(FTask, 'Ok', mlNormal)
      else
        ExitMessage := TExitMessage.Create(FTask, 'Aborted', mlAbort);
    except on E :Exception do
      ExitMessage := TExitMessage.Create(FTask, E.Message, mlFatal);
    end;
    FContext.FMessages.Push(ExitMessage);
    Suspended := true;
  end;
end;

{ TWorkers }

function TWorkers.Drop: boolean;
var
  First :TWorker;
begin
  if Peek(First) then
     First.Free;
  result := inherited Drop;
end;

{ TContext }

function TContext.GetAborted: boolean;
begin
  result := FDispatcher.FAborted;
end;

constructor TContext.Create(Dispatcher: TDispatcher);
begin
  FDispatcher := Dispatcher;
  FMessages := TMessages.Create;
end;

destructor TContext.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

{ TDispatcher }

procedure TDispatcher.Print(Sender: TCustomTask; const Text: string;
  Level: TLevel);
begin
  if Assigned(FOnPrint) then
    FOnPrint(Sender, Sender.Worker.Id, Text, Level);
end;

procedure TDispatcher.Progress(Sender: TCustomTask; Steps :integer);
begin
  inc(FStepCount, Steps);
  if Assigned(FOnProgress) then
    FOnProgress(self, FStepCount/FTotalSteps);
end;

constructor TDispatcher.Create;
begin
  FStopOnError := true;
  FMaxWorkerCount := 0;
end;

destructor TDispatcher.Destroy;
begin
  inherited Destroy;
end;

function TDispatcher.Execute(Tasks: TTasks): boolean;
var
  Pool :TWorkers;             // Queue of ready workers
  PoolCapacity :integer;      // Maximum Number of workers
  Working :array of TWorker;  // List of currently working workers, Index is Id
  WorkingCount :integer;      // Number of valid entries in Working = Pool.Capacity - Pool.Count
  Worker :TWorker;            // A current worker
  Context :TContext;
  Msg :TMessage;
  Id :integer;
  NextTask :integer;
begin
  // Is there anything to do?
  if Tasks.Count=0 then
    Exit(true);

  Pool := nil;
  Context := TContext.Create(self);
  try
    Context.FDispatcher := self;

    // Prepare progress system
    FTotalSteps := Tasks.GetTotalSteps;
    FStepCount := 0;

    // Calculate number of needed workers
    case MaxWorkerCount of
    0: // default
      PoolCapacity := TThread.ProcessorCount;
    else
      begin
        PoolCapacity := MaxWorkerCount;
        if PoolCapacity>TThread.ProcessorCount then
          PoolCapacity := TThread.ProcessorCount;
      end;
    end;
    if PoolCapacity>Tasks.Count then
      PoolCapacity := Tasks.Count;

    // Hire some workers
    Pool := TWorkers.Create(PoolCapacity);
    for Id := 0 to PoolCapacity-1 do
      Pool.Push(TWorker.Create(Context, Id));

    // Initiate the list of working workers Ids
    SetLength(Working, PoolCapacity);
    WorkingCount := 0;

    ////////////////////////////////////////////////////////////////////////////
    // Assign Tasks to Workers and handle their messages
    NextTask := 0;
    while true do begin

      // If a task is available then assign it to a free worker
      if not FAborted and (NextTask<Tasks.Count) and Pool.Pop(Worker) then begin
        Working[Worker.Id] := Worker;
        inc(WorkingCount);
        Worker.Start(Tasks[NextTask]);
        inc(NextTask);
      end;

      // Handle worker messages
      if Context.FMessages.Empty then
        Sleep(0);
      while Context.FMessages.Pop(Msg) do begin
        try
          if Msg is TPrintMessage then with TPrintMessage(Msg) do begin
            self.Print(Task, Text, Level);
            if Msg is TExitMessage then with TExitMessage(Msg) do begin
              // Waiting for the tasks worker to become suspended
              while not Task.Worker.Suspended do Sleep(0);
              // Adding newly available worker to the pool
              Pool.Push(Task.Worker);
              // Remove him from the list of working
              Working[Task.Worker.Id] := nil;
              dec(WorkingCount);

              // If all workers finished their work and aborted then finish
              if (WorkingCount=0) and FAborted then
                Exit(false);

              // If all Tasks are done and no worker is currently running then finish
              if (NextTask=Tasks.Count) and (WorkingCount=0) then
                Exit(true);

              // When error occured and not already aborting and StopOnError then stop all workers
              if (Level in [mlAbort, mlFatal]) and not FAborted and StopOnError then
                // Terminate running workers and continue waiting for their ExitMessage
                Abort;
            end;
          end else if Msg is TProgressMessage then with TProgressMessage(Msg) do begin
            self.Progress(Task, Steps);
          end;
        finally
          Msg.Free;
        end;
      end;
    end;
    result := true;
  finally
    Context.Free;
    // Fire all workers
    while Pool.Pop(Worker) do
      Worker.Free;
    Pool.Free;
  end;
end;

procedure TDispatcher.Abort;
begin
  FAborted := true;
end;

end.

