unit logging;

interface

uses SysUtils;

type

  TLogLevel = (llNone=-2, llDebug=-1, llHint=0, llInfo=1, llWarning=2, llError=3, llCrash=4, llSpecial=5);
  TLogFlag = (lfApp, lfUser, lfSystem);

  TLogger = class
  public
    procedure Log(const Line :string; Level :TLogLevel = llInfo); virtual; abstract;
    class function GetDefaultLogger :TLogger; static;
    class procedure SetDefaultLogger(Value :TLogger); static;
    class property DefaultLogger :TLogger read GetDefaultLogger write SetDefaultLogger;
  end;

function IsLog :boolean;
procedure Log(const Line :string; Level :TLogLevel = llInfo); overload;
procedure Log(const Fmt :string; const Args :array of const; Level :TLogLevel = llInfo); overload;
procedure Warning(const Line :string); overload;
procedure Warning(const Fmt :string; const Args :array of const); overload;
procedure Hint(const Line :string); overload;
procedure Hint(const Fmt :string; const Args :array of const); overload;

implementation

var
  DefaultLogger :TLogger = nil;

function IsLog :boolean;
begin
  result := Assigned(DefaultLogger);
end;

procedure Log(const Line :string; Level :TLogLevel);
begin
  if IsLog then
    DefaultLogger.Log(Line, Level);
end;

procedure Log(const Fmt :string; const Args :array of const; Level :TLogLevel = llInfo); overload;
begin
  if IsLog then
    DefaultLogger.Log(Format(Fmt, Args), Level);
end;

procedure Warning(const Line: string);
begin
  Log(Line, llWarning);
end;

procedure Warning(const Fmt: string; const Args: array of const);
begin
  Log(Fmt, Args, llWarning);
end;

procedure Hint(const Line: string);
begin
  Log(Line, llHint);
end;

procedure Hint(const Fmt: string; const Args: array of const);
begin
  Log(Fmt, Args, llHint);

end;

{ TLogger }

class function TLogger.GetDefaultLogger: TLogger;
begin
  result := Logging.DefaultLogger;
end;

class procedure TLogger.SetDefaultLogger(Value: TLogger);
begin
  if Value<>Logging.DefaultLogger then begin
    FreeAndNil(Logging.DefaultLogger);
    Logging.DefaultLogger := Value;
  end;
end;

initialization
begin
end;

finalization
begin
  if Assigned(DefaultLogger) then
    FreeAndNil(DefaultLogger);
end;

end.
