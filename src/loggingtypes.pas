{$mode delphi}

unit loggingtypes;

interface

uses
  Classes, logging, signalslot;

type
  TLogEvent = procedure(Sender :TObject; const Lines :String; Level :TLogLevel; Scope :TLogFlag) of object;

  TLogSignal<TSender :Class> = class(TSignal<TSender, TLogEvent>)
    procedure Notify(const Line :string; Level :TLogLevel; Scope :TLogFlag = lfApp);
  end;

const
  LOGLEVELSTR :array[TLogLevel] of string = ('', 'DEBUG', 'HINT', 'INFO', 'WARNING', 'ERROR', 'CRASH', 'SPECIAL');


implementation

{ TLogSignal<TSender> }

procedure TLogSignal<TSender>.Notify(const Line: string; Level: TLogLevel; Scope :TLogFlag);
var
  i :TLogSignal<TSender>.TSignalLink;
begin
  for i in self do
    i.Callback(Sender, Line, Level, Scope);
end;

end.
