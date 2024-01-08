unit templateengine;

{$mode delphi}

interface

uses
  Classes, SysUtils, Generics.Collections;

type

  TDelimiters = record
    Del1, Del2 :string;
  end;

  const
    PERCENTDELIMITERS :TDelimiters = (Del1: '%'; Del2: '%');
    BROSTDELIMITERS :TDelimiters = (Del1: '«'; Del2: '»');
    MUSTACHEDELIMITERS :TDelimiters = (Del1: '{{'; Del2: '}}');

type
  { TSolver }

  TSolver = class
  public type
    TStats = record
      Dependencies :integer;
      Solved :integer;
      Unknown :integer;
    end;
  private type
    TRec = class
      Key :string;
      Value :string;
      Refs :array of TRec;
      DepsCount :integer;  // Number of vars which do refer to this
    end;

    { TIterator }

    TIterator = record
      Solver :TSolver;
      Subject :string;
      i0, i1 :integer;
      function Next(out Key :string) :boolean;
      constructor Create(Solver :TSolver; const Subject :string);
    end;
  private
    FDict :TDictionary<string, TRec>;
    FArray :TObjectList<TRec>;
    FD1 :string;
    FD2 :string;
    function GetCount: integer;
    function GetItem(const Key :string): string;
    function Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
    procedure SetItem(const Key :string; const Value: string);
  public
    constructor Create(const Delimiters :TDelimiters);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key, Value :string);
    procedure Reload(Index :integer; const Value :string);
    function Solve(out Stats :TStats) :boolean;
    function Replace(const Subject :string) :string;
    function TryGetValue(const Key :string; out Value :string) :boolean;
    function GetValue(const Key :string) :string; overload;
    function GetValue(Index :integer) :string; overload;
    property Count :integer read GetCount;
    property Items[const Key :string] :string read GetItem write SetItem; default;
  end;

implementation  // »SIZE«      «SIZE»

uses
  StrUtils;

resourcestring
  SErrVarNotFoundFmt = 'Variable ''%s'' not found in VarSolver.';

{ TSolver }

function TSolver.GetCount: integer;
begin
  result := FArray.Count;
end;

function TSolver.GetItem(const Key :string): string;
begin
  result := FDict[Key].Value;
end;

procedure TSolver.SetItem(const Key :string; const Value :string);
var
  Rec :TRec;
begin
  if FDict.TryGetValue(Key, Rec) then
    Rec.Value := Value
  else
    Add(Key, Value);
end;

constructor TSolver.Create(const Delimiters :TDelimiters);
begin
  FDict := TDictionary<string, TRec>.Create;
  FArray := TObjectList<TRec>.Create;
  FD1 := Delimiters.Del1;
  FD2 := Delimiters.Del2;
end;

destructor TSolver.Destroy;
begin
  FDict.Free;
  FArray.Free;
  inherited Destroy;
end;

procedure TSolver.Clear;
begin
  FDict.Clear;
  FArray.Clear;
end;

procedure TSolver.Add(const Key, Value: string);
var
  Rec :TRec;
begin
  Rec := TRec.Create;
  try
    Rec.Key := Key;
    Rec.Value := Value;
    FDict.Add(Key, Rec);
  except
    Rec.Free;
    raise;
  end;
  FArray.Add(Rec);
end;

procedure TSolver.Reload(Index: integer; const Value: string);
begin
  FArray[Index].Value := Value;
end;

function TSolver.Solve(out Stats: TStats): boolean;
var
  Iterator :TIterator;
  l, r, d :TRec;
  q :string;
  Key :string;
  found :boolean;
  n :integer;
begin
  Stats.Dependencies := 0;
  Stats.Solved := 0;
  Stats.Unknown := 0;
  if Count=0 then Exit(true);

  // Find all Dependencies
  for l in FArray do begin
    Iterator := TIterator.Create(self, l.Value);
    while Iterator.Next(Key) do begin
      if FDict.TryGetValue(Key, r) then begin
        // Check, if the dependency is counted only once
        found := false;
        for d in r.Refs do begin
          found := d=l;
          if found then break;
        end;
        if not found then begin
          inc(Stats.Dependencies);
          inc(l.DepsCount);
          SetLength(r.Refs, Length(r.Refs)+1);
          r.Refs[High(r.Refs)] := l;
        end;
      end else
        inc(Stats.Unknown);
    end;
  end;

  // Solve all vars until no dependencies
  repeat
    n := Stats.Dependencies;
    for l in FArray do if l.DepsCount=0 then begin
      for r in l.Refs do begin
        r.Value := StringReplace(r.Value, FD1+l.Key+FD2, l.Value, [rfReplaceAll]);
        dec(r.DepsCount);
        dec(Stats.Dependencies);
        inc(Stats.Solved);
      end;
      l.Refs := nil;
    end;
  until (stats.Dependencies=0) or (n=Stats.Dependencies);
  result := stats.Dependencies=0;
end;

function TSolver.Replace(const Subject: string): string;
var
  Iterator :TIterator;
  Found :boolean;
  Key :string;
  Rec :TRec;
  p :integer; // Points to the top of the reaulting string
begin
  Found := false;
  Iterator := TIterator.Create(self, Subject);
  result := '';
  p := 1;
  while Iterator.Next(Key) do begin
    if FDict.TryGetValue(Key, Rec) then begin
      Found := true;
      result := result + Copy(Subject, p, Iterator.i0-p) + Rec.Value;
      p := Iterator.i1+Length(FD2);
    end;
  end;
  if Found then
    result := result + Copy(Subject, p, Length(Subject)-p+1)
  else
    Exit(Subject);
end;

function TSolver.TryGetValue(const Key: string; out Value: string): boolean;
var
  Rec :TRec;
begin
  result := FDict.TryGetValue(Key, Rec);
  if result then
    Value := Rec.Value;
end;

function TSolver.GetValue(const Key: string): string;
begin
  if not TryGetValue(Key, result) then
    raise Exception.CreateFmt(SErrVarNotFoundFmt, [Key]);
end;

function TSolver.GetValue(Index: integer): string;
begin
  result := FArray[Index].Value;
end;

function TSolver.Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
begin
  with Iterator do begin
    i0 := PosEx(FD1, Txt, i1+1);
    result := i0 > 0;
    if result then begin
      i1 := PosEx(FD2, Txt, i0+Length(FD1));
      result := i1 > 0;
      if result then
        Key := Copy(Txt, i0+Length(FD1), i1-i0-Length(FD1));
    end;
  end;
end;

{ TSolver.TIterator }

function TSolver.TIterator.Next(out Key: string): boolean;
begin
  i0 := PosEx(Solver.FD1, Subject, i1+1);
  result := i0 > 0;
  if result then begin
    i1 := PosEx(Solver.FD2, Subject, i0+Length(Solver.FD1));
    result := i1 > 0;
    if result then
      Key := Copy(Subject, i0+Length(Solver.FD1), i1-i0-Length(Solver.FD1));
  end;
end;

constructor TSolver.TIterator.Create(Solver: TSolver; const Subject :string);
begin
  self.Solver := Solver;
  self.Subject := Subject;
  i0 := 0;
  i1 := 0;
end;

//procedure Test;
//var
//  Solver :TSolver;
//  Stats :TSolver.TStats;
//begin
//  Solver := TSolver.Create(PERCENTDELIMITERS);
//  Solver.Add('v1', '1234');
//  Solver.Add('v2', 'abcd+%v1%');
//  Solver.Add('v3', '%v4%XYZ%v5%+%v1%');
//  Solver.Add('v4', '4444x%v2%');
//  Solver.Add('v5', 'aaaaa%v6%');
//  Solver.Add('v6', '%v5%eeeeee');
//  Solver.Solve(Stats);
//end;
//
//initialization
//begin
//  Test;
//end;
//
end.

