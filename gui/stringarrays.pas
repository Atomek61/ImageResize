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
    procedure Add(const Value :string);
    procedure Clear;
    function Contains(const Value: string): boolean;
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

procedure TStringArrayHelper.Add(const Value: string);
begin
  SetLength(self, Length(self)+1);
  self[High(self)] := Value;
end;

procedure TStringArrayHelper.Clear;
begin
  self := nil;
end;

function TStringArrayHelper.Contains(const Value: string): boolean;
var
  i :integer;
begin
  for i:=0 to High(self) do
    if SameText(Value, self[i]) then Exit(true);
  result := false;
end;

end.

