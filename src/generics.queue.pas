unit generics.queue;

{$mode delphi}

interface

uses
  Classes, SysUtils;

type

  { TCustomQueue }

  TCustomQueue<T> = class abstract
  private
    FItems :array of T;
    FCount :integer;
    FTail :integer;
    FHead :integer;
    function GetAvailable: boolean;
    function GetEmpty: boolean;
    function GetItem(Index: integer): T;
    procedure Pack;
    procedure Grow;
    function GetFull: boolean;
    function GetCapacity: integer;
    procedure SetCapacity(const Value: integer);
  protected
    function GetCount: integer; virtual;
    function Drop :boolean; virtual;
    property Items[Index :integer] :T read GetItem; default;
    property Capacity :integer read GetCapacity write SetCapacity;
    property Count :integer read GetCount;
    property Full :boolean read GetFull;
    property Empty :boolean read GetEmpty;
    property Available :boolean read GetAvailable; // = not empty
  public
    constructor Create(ACapacity :integer = 0); // Kann 0 sein, dann unbegrenztes Wachstum
    destructor Destroy; override;
    procedure Push(Item :T); virtual;
    function Pop(out Item :T) :boolean; virtual;
    procedure Clear; virtual;
  end;

  TQueue<T> = class(TCustomQueue<T>)
  public
    function Peek(out Item :T) :boolean;
    property Items;
    property Capacity;
    property Count;
    property Full;
    property Empty;
    property Available; // = not empty
  end;

implementation

{ TCustomQueue }

constructor TCustomQueue<T>.Create(ACapacity: integer);
begin
  FCount := 0;
  SetLength(FItems, ACapacity);
  FTail := 0;
end;

destructor TCustomQueue<T>.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TCustomQueue<T>.GetEmpty: boolean;
begin
  result := FCount = 0;
end;

function TCustomQueue<T>.GetAvailable: boolean;
begin
  result := Count>0;
end;

function TCustomQueue<T>.GetCount: integer;
begin
  result := FCount;
end;

function TCustomQueue<T>.GetItem(Index: integer): T;
begin
  result := FItems[(FTail + Index) mod Length(FItems)];
end;

procedure TCustomQueue<T>.Pack;
var
  Tmp :array of T;
  i, n :integer;
begin
  if FCount>0 then begin
    SetLength(Tmp, Count);
    n := Length(FItems);
    for i:=0 to FCount-1 do
      Tmp[i] := FItems[(FTail+i) mod n];
    FItems := Tmp;
  end;
  FTail := 0;
  FHead := FCount;
end;

procedure TCustomQueue<T>.Grow;
begin
  if Capacity=0 then
    Capacity := 4
  else
    SetCapacity(Capacity * 2);
end;

function TCustomQueue<T>.GetFull: boolean;
begin
  result := Count = Length(FItems);
end;

function TCustomQueue<T>.GetCapacity: integer;
begin
  result := Length(FItems);
end;

procedure TCustomQueue<T>.SetCapacity(const Value: integer);
begin
  if (Value<0) or (Value=Capacity) then Exit;
  Pack;
  while FCount>Value do
    Drop;
  SetLength(FItems, Value);
end;

procedure TCustomQueue<T>.Push(Item: T);
begin
  if Full then
    Grow;
  inc(FCount);
  FItems[FHead] := Item;
  FHead := (FHead + 1) mod Length(FItems);
end;

function TCustomQueue<T>.Pop(out Item: T): boolean;
begin
  result := FCount>0;
  if result then begin
    Item := FItems[FTail];
    dec(FCount);
    FTail := (FTail+1) mod Length(FItems);
  end;
end;

function TCustomQueue<T>.Drop :boolean;
begin
  result := Count>0;
  if result then begin
    dec(FCount);
    FTail := (FTail+1) mod Length(FItems);
  end;
end;

procedure TCustomQueue<T>.Clear;
begin
  while Drop do;
  FTail := 0;
  FHead := 0;
end;

{ TQueue<T> }

function TQueue<T>.Peek(out Item: T): boolean;
begin
  result := FCount>0;
  if result then
    Item := Items[Count-1];
end;

end.

