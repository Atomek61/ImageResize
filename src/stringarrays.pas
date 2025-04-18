unit stringarrays;

{$mode delphi}
{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils;

type
  { TStringArrayHelper }

  TStringArrayHelper = type helper for TStringArray
  private
    function GetCount: integer;
    procedure SetCount(AValue: integer);
  public
    function IsEqual(const o :TStringArray) :boolean;
    procedure Add(const Value :string);
    procedure Clear;
    function TryFind(const Value :string; out Index :integer; IgnoreCase :boolean = false) :boolean;
    function Contains(const Value: string; IgnoreCase :boolean = false): boolean;
    function Join(const Delimiter :string) :string;
    property Count :integer read GetCount write SetCount;
  end;

implementation

{ TStringArrayHelper }

function TStringArrayHelper.GetCount: integer;
begin
  result := Length(self);
end;

procedure TStringArrayHelper.SetCount(AValue: integer);
begin
  SetLength(self, AValue);
end;

function TStringArrayHelper.IsEqual(const o: TStringArray): boolean;
var
  i :integer;
begin
  result := o.Count = Count;
  if result then
    for i:=0 to High(o) do begin
      result := o[i]<>self[i];
      if not result then break;
    end;
end;

procedure TStringArrayHelper.Add(const Value: string);
begin
  SetLength(self, Length(self)+1);
  self[High(self)] := Value;
end;

procedure TStringArrayHelper.Clear;
begin
  self := nil;
end;

function TStringArrayHelper.TryFind(const Value: string; out Index: integer; IgnoreCase: boolean): boolean;
var
  i :integer;
begin
  if IgnoreCase then begin
    for i:=0 to High(self) do
      if SameText(Value, self[i]) then begin
        Index := i;
        Exit(true);
      end;
  end else begin
    for i:=0 to High(self) do
      if Value=self[i] then begin
        Index := i;
        Exit(true);
      end;
  end;
  result := false;
end;

function TStringArrayHelper.Contains(const Value: string; IgnoreCase: boolean): boolean;
var
  Index :integer;
begin
  result := TryFind(Value, Index, IgnoreCase);
end;

function TStringArrayHelper.Join(const Delimiter: string): string;
var
  i :integer;
begin
  if Count = 0 then Exit('');
  result := self[0];
  for i:=1 to Count-1 do
    result := result + Delimiter + self[i];
end;

//procedure TStringArrayHelper.Split(const Str: string);
//begin
//
//end;
//
end.

