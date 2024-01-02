{$mode delphi}

unit signalslot;

interface

uses
  Classes;

type
  // Base for all classes here
  TLink = class
  private
    FPrev :TLink;
    FNext :TLink;
  protected
    procedure Bind(Link :TLink; First :boolean);
    procedure Unbind;
  public
    constructor Create;
    destructor Destroy; override;
    function First(var Link): boolean;
    function Next(var Link): boolean;
  end;

  // Base class for connection between two linked lists
  TCrosslink = class(TLink)
  private
    FUsage :integer;
    FCounterpart :TCrosslink;
    procedure Use;
    procedure Unuse;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // A chain of Crosslinks, the first element controls the chains links
  TChain = class(TLink)
  protected type

    TEnumerator = class
    private
      FChain :TChain;
      FCurrent :TCrossLink;
    public
      constructor Create(Chain :TChain);
      property Current :TCrossLink read FCurrent;
      function MoveNext :boolean;
      destructor Destroy; override;
    end;

  public
    destructor Destroy; override;
    procedure Clear;
    function Count :integer;
    function Empty :boolean;
    function GetEnumerator :TEnumerator; reintroduce;
  end;

  TSignal<TSender :Class> = class(TChain)
  private
    FSender :TSender;
  public
    constructor Create(Sender :TSender);
    property Sender :TSender read FSender;
  end;

  // The slot-part of the crosslink. A Slot can reference multiple signals
  TSlot = class(TChain)
  end;

  TSlotLink = class(TCrossLink)
    Slot :TSlot;
  end;

  TSignal<TSender :Class; TCallback> = class(TSignal<TSender>)
  protected type

    TSignalLink = class(TCrossLink)
    private
      FCallback :TCallback;
    public
      property Callback :TCallback read FCallback;
    end;

    TEnumerator = class(TChain.TEnumerator)
    private
      function GetCurrent: TSignalLink;
    public
      property Current :TSignalLink read GetCurrent;
    end;

  public
    function Register(Slot :TSlot; Callback :TCallback; First :boolean = false) :TSlotLink;
    function GetEnumerator :TEnumerator; reintroduce;
  end;

  // Simple OnNotify-like signal
//  TNotifyMethod = ;
  TNotifySignal = class(TSignal<TObject, TNotifyEvent>)
  public
    procedure Notify;
  end;


implementation

{ TLink }

constructor TLink.Create;
begin
  FPrev := self;
  FNext := self;
end;

destructor TLink.Destroy;
begin
  Unbind;
  inherited;
end;

function TLink.First(var Link): boolean;
begin
  result := FNext<>self;
  if result then
    TLink(Link) := FNext;
end;

function TLink.Next(var Link): boolean;
begin
  TLink(Link) := TLink(Link).FNext;
  result := TLink(Link)<>self;
end;

procedure TLink.Unbind;
begin
  FPrev.FNext := FNext;
  FNext.FPrev := FPrev;
  FNext := self;
  FPrev := self;
end;

procedure TLink.Bind(Link: TLink; First :boolean);
begin
  Unbind;

  if First then begin

    FNext := Link.FNext;
    FPrev := Link;

    Link.FNext := self;
    FNext.FPrev := self;

  end else begin

    FNext := Link;
    FPrev := Link.FPrev;

    Link.FPrev := self;
    FPrev.FNext := self;

  end;
end;

{ TChain }

procedure TChain.Clear;
begin
  while FPrev<>self do
    FPrev.Free;
end;

function TChain.Count: integer;
var
  Link :TLink;
begin
  result := 0;
  Link := FNext;
  while (Link<>self) do begin
    inc(result);
    Link := Link.FNext
  end;
end;

destructor TChain.Destroy;
begin
  Clear;
  inherited;
end;

function TChain.Empty: boolean;
begin
  result := FPrev=FNext;
end;

function TChain.GetEnumerator: TEnumerator;
begin
  result := TEnumerator.Create(self);
end;

{ TChain.TEnumerator }

constructor TChain.TEnumerator.Create(Chain: TChain);
begin
  FChain := Chain;
  FCurrent := nil;
end;

destructor TChain.TEnumerator.Destroy;
begin
  if Assigned(FCurrent) then
    FCurrent.Unuse;
  inherited;
end;

function TChain.TEnumerator.MoveNext: boolean;
var
  NextCurrent :TLink;
begin
  if not Assigned(FCurrent) then begin
    result := FChain.FNext <> FChain;
    if result then begin
      FCurrent := TCrossLink(FChain.FNext);
      FCurrent.Use;
    end;
  end else begin
    NextCurrent := FCurrent.FNext;
    FCurrent.Unuse;
    FCurrent := nil;
    result := NextCurrent<>FChain;
    if result then begin
      FCurrent := TCrossLink(NextCurrent);
      FCurrent.Use;
    end;
  end;
end;

{ TCrosslink }

constructor TCrosslink.Create;
begin
  inherited;
  Use;
end;

destructor TCrosslink.Destroy;
var
  DanglingCounterpart :TCrossLink;
begin
  DanglingCounterpart := FCounterpart;
  if Assigned(DanglingCounterpart) then begin
    DanglingCounterpart.FCounterpart := nil;
    DanglingCounterpart.Unuse;
  end;
  inherited;
end;

procedure TCrosslink.Unuse;
begin
  dec(FUsage);
  if FUsage<=0 then
    Free;
end;

procedure TCrosslink.Use;
begin
  inc(FUsage);
end;

function TSignal<TSender, TCallback>.GetEnumerator: TEnumerator;
begin
  result := TEnumerator(inherited GetEnumerator);
end;

function TSignal<TSender; TCallback>.Register(Slot :TSlot; Callback: TCallback; First :boolean) :TSlotLink;
var
  SignalLink :TSignalLink;
  SlotLink :TSlotLink;
begin
  // Die beiden neuen Links erzeugen
  SignalLink := TSignalLink.Create;
  SignalLink.FCallback := Callback;
  SlotLink := TSlotLink.Create;

  // Die neuen Links miteinander verbinden
  SignalLink.FCounterpart := SlotLink;
  SlotLink.FCounterpart := SignalLink;

  // Die neuen Links in die Ketten ihrer jeweiligen Seite binden
  SlotLink.Bind(Slot, false);
  SignalLink.Bind(self, First);

  result := SlotLink;
end;

{ TSignal<TSender, TCallback>.TEnumerator }

function TSignal<TSender, TCallback>.TEnumerator.GetCurrent: TSignalLink;
begin
  result := TSignalLink(inherited Current);
end;

{ TNotifySignal }

procedure TNotifySignal.Notify;
var
  i :TSignalLink;
begin
  for i in self do
    i.Callback(Sender);
end;

{ TSignal<TSender> }

constructor TSignal<TSender>.Create(Sender: TSender);
begin
  inherited Create;
  FSender := Sender;
end;

end.
