unit loggingconsole;

interface

uses
  logging, loggingtypes;

type
  TConsoleLogger = class(TLogger)
  private
    FMinLevel :TLogLevel;
    FPrefixed :boolean;
  public
    constructor Create;
    procedure Log(const Line :string; Level :TLogLevel = llInfo); override;
    property MinLevel :TLogLevel read FMinLevel write FMinLevel;
    property Prefixed :boolean read FPrefixed write FPrefixed;
  end;

implementation

uses
  datetimeutils;

{ TConsoleLogger }

constructor TConsoleLogger.Create;
begin
  inherited;
  FMinLevel := llHint;
end;

procedure TConsoleLogger.Log(const Line: string; Level: TLogLevel);
begin
  inherited;
  if Level>=FMinLevel then begin
    if FPrefixed then
      WriteLn(DateTimeToISOStr(UTCNow)+' '+LOGLEVELSTR[Level]+': '+Line)
    else
      WriteLn(Line);
  end;
end;

initialization
begin
  TLogger.SetDefaultLogger(TConsoleLogger.Create);
end;

end.
