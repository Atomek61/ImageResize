unit templateengine;

{$mode delphi}
// TSolver is a class which reduces dependencies between vaiables.
// A given list of names/values may contain references to other name/value-
// pairs, i.e. FILENAME="img{SIZE}.{EXT}" refers to the other variables
// SIZE and EXT. Those may againn refer to other variables.
// Solving means to replace the references in a way that at the end even nested
// dependencies are removed.

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils;

type

  { TDelimiters }

  TDelimiters = record
    Del1, Del2 :string;
    constructor Create(const Del1, Del2 :string);
    function toString :string;
    class function TryStrToDelimiters(const Value :string; out Delimiters :TDelimiters) :boolean; static;
    class function StrToDelimiters(const Value :string) :TDelimiters; static;
    function UpgradeFrom(const Subject :string; const Delimiters :TDelimiters) :string;
  end;

  TTypeDelimiters = TDictionary<string, TDelimiters>; // Pairs of fileytype/delimiters ('css', <,>), (html, {,}), ('js', %,%)

  const
    PERCENTDELIMITERS       :TDelimiters = (Del1: '%'; Del2: '%');
    BROSTDELIMITERS         :TDelimiters = (Del1: '«'; Del2: '»');
    MUSTACHEDELIMITERS      :TDelimiters = (Del1: '{{'; Del2: '}}');
    CURLYBRACEDELIMITERS    :TDelimiters = (Del1: '{'; Del2: '}');

    MAXKEYLENGTH = 24;

type
  { TSolver }

  TSolver = class
  public type
    TStats = record
      LeftDependencies :integer; // Is 0 after successfull solving all
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
      procedure NoMatch; // If Key is not that what you wanted, then search from i0+1
      constructor Create(Solver :TSolver; const Subject :string);
    end;
  private
    FDict :TDictionary<string, TRec>;
    FArray :TObjectList<TRec>;
    function GetCount: integer;
    function GetItem(const Key :string): string;
    function GetKey(Index :integer): string;
    function GetValue(Index :integer) :string; overload;
    //function Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
    procedure SetItem(const Key :string; const Value: string);
  public
    Delimiters :TDelimiters;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key :string; const Value :string = '');
    procedure Load(const Key :string; const Value :string);
    procedure Reload(Index :integer; const Value :string);
    function TrySolve(out Stats :TStats) :boolean;
    procedure Solve(out Stats :TStats);
    function Replace(const Subject :string; out Replacements :integer) :string;
    function TryGetValue(const Key :string; out Value :string) :boolean;
    function GetValue(const Key :string) :string; overload;
    property Count :integer read GetCount;
    property Items[const Key :string] :string read GetItem write SetItem; default;
    property Keys[Index :integer] :string read GetKey;
    property Values[Index :integer] :string read GetValue;
  end;


implementation  // »SIZE«      «SIZE»

resourcestring
  SErrVarNotFoundFmt = 'Variable ''%s'' not found in VarSolver.';
  SErrInvalidDelimitersFmt = 'Invalid delimiters ''%s''.';
  SErrSolving = 'Cant solve valiables (circular dependencies?)';

{ TDelimiters }

constructor TDelimiters.Create(const Del1, Del2: string);
begin
  self.Del1 := Del1;
  self.Del2 := Del2;
end;

function TDelimiters.toString: string;
begin
  result := Del1+','+Del2;
end;

class function TDelimiters.TryStrToDelimiters(const Value: string; out Delimiters: TDelimiters): boolean;
var
  s :TStringArray;
begin
  s := Value.Split(',');
  if Length(s)<>2 then Exit(false);
  Delimiters.Del1 := Trim(s[0]);
  Delimiters.Del2 := Trim(s[1]);
  result := (Delimiters.Del1<>'') and (Delimiters.Del2<>'');
end;

class function TDelimiters.StrToDelimiters(const Value: string): TDelimiters;
begin
  if not TryStrToDelimiters(Value, result) then
    raise Exception.CreateFmt(SErrInvalidDelimitersFmt, [Value]);
end;

function TDelimiters.UpgradeFrom(const Subject: string; const Delimiters: TDelimiters): string;
var
  i1, i2, i0 :integer;
  n, n1, n2 :integer;
begin
  result := '';
  i1 := 1;
  i2 := 1;
  n := Length(Subject);
  n1 := Length(Delimiters.Del1);
  n2 := Length(Delimiters.Del2);
  while true do begin
    i0 := i1;
    i1 := PosEx(Delimiters.Del1, Subject, i0);
    if i1=0 then begin
      if result = '' then
        Exit(Subject)
      else
        Exit(result + Copy(Subject, i0, n-i0+1));
    end else begin
      i2 := PosEx(Delimiters.Del2, Subject, i1+n1);
      if i2=0 then Exit(result + Copy(Subject, i0, n-i0+1));
      result := result + Copy(Subject, i0, i1-i0) + Del1 + Copy(Subject, i1+n1, i2-i1-n1) + Del2;
      i1 := i2 + n2;
    end;
  end;
end;

{ TSolver }

function TSolver.GetCount: integer;
begin
  result := FArray.Count;
end;

function TSolver.GetItem(const Key :string): string;
begin
  result := FDict[Key].Value;
end;

function TSolver.GetKey(Index : integer): string;
begin
  result := FArray[Index].Key;
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

constructor TSolver.Create;
begin
  FDict := TDictionary<string, TRec>.Create;
  FArray := TObjectList<TRec>.Create;
  Delimiters := CURLYBRACEDELIMITERS;
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

procedure TSolver.Add(const Key :string; const Value :string);
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

procedure TSolver.Load(const Key: string; const Value: string);
var
  Rec :TRec;
begin
  if FDict.TryGetValue(Key, Rec) then
    Rec.Value := Value
  else
    Add(Key, Value);
end;

procedure TSolver.Reload(Index: integer; const Value: string);
begin
  FArray[Index].Value := Value;
end;

function TSolver.TrySolve(out Stats: TStats): boolean;
var
  Iterator :TIterator;
  l, r, d :TRec;
  Key :string;
  found :boolean;
  Dependencies :integer;
begin
  Stats.LeftDependencies := 0;
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
          inc(Stats.LeftDependencies);
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
    Dependencies := Stats.LeftDependencies;
    for l in FArray do if l.DepsCount=0 then begin
      for r in l.Refs do begin
        r.Value := StringReplace(r.Value, Delimiters.Del1+l.Key+Delimiters.Del2, l.Value, [rfReplaceAll]);
        dec(r.DepsCount);
        dec(Stats.LeftDependencies);
        inc(Stats.Solved);
      end;
      l.Refs := nil;
    end;
  until (stats.LeftDependencies=0) or (Dependencies=Stats.LeftDependencies);
  result := stats.LeftDependencies=0;
end;

procedure TSolver.Solve(out Stats: TStats);
begin
  if not TrySolve(Stats) then
    raise Exception.Create(SErrSolving);
end;

function TSolver.Replace(const Subject: string; out Replacements :integer): string;
var
  Iterator :TIterator;
  Key :string;
  Rec :TRec;
  p :integer; // Points to the top of the reaulting string
begin
  Replacements := 0;
  Iterator := TIterator.Create(self, Subject);
  result := '';
  p := 1;
  while Iterator.Next(Key) do begin
    if FDict.TryGetValue(Key, Rec) then begin
      inc(Replacements);
      result := result + Copy(Subject, p, Iterator.i0-p) + Rec.Value;
      p := Iterator.i1+Length(Delimiters.Del2);
    end;
  end;
  if Replacements>0 then
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

//function TSolver.Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
//begin
//  with Iterator do begin
//    i0 := PosEx(Delimiters.Del1, Txt, i1+1);
//    result := i0 > 0;
//    if result then begin
//      i1 := PosEx(Delimiters.Del2, Txt, i0+Length(Delimiters.Del1));
//      result := i1 > 0;
//      if result then
//        Key := Copy(Txt, i0+Length(Delimiters.Del1), i1-i0-Length(Delimiters.Del1));
//    end;
//  end;
//end;
//
{ TSolver.TIterator }

function TSolver.TIterator.Next(out Key: string): boolean;
var l :integer;
begin
  while true do begin
    i0 := PosEx(Solver.Delimiters.Del1, Subject, i1+Length(Solver.Delimiters.Del2));
    if i0 = 0 then Exit(false);
    i1 := PosEx(Solver.Delimiters.Del2, Subject, i0+Length(Solver.Delimiters.Del1));
    if i1 = 0 then Exit(false);
    l := i1-i0-Length(Solver.Delimiters.Del1);
    if l>MAXKEYLENGTH then
      i1 := i0 + Length(Solver.Delimiters.Del1) - Length(Solver.Delimiters.Del2)
    else begin
      Key := Copy(Subject, i0+Length(Solver.Delimiters.Del1), l);
      Exit(True);
    end;
  end;
end;

procedure TSolver.TIterator.NoMatch;
begin
  i1 := i0 + Length(Solver.Delimiters.Del1) - Length(Solver.Delimiters.Del2);
end;

constructor TSolver.TIterator.Create(Solver: TSolver; const Subject :string);
begin
  self.Solver := Solver;
  self.Subject := Subject;
  i0 := 1;
  i1 := 1-Length(Solver.Delimiters.Del2);
end;

//procedure Test;
//var
//  s1, s2 :string;
//  d1, d2 :TDelimiters;
//begin
//  d1 := TDelimiters.Create('<', '>>');
//  d2 := TDelimiters.Create('[', ']');
//  s2 := d2.UpgradeFrom('Das<SIZE>>ist<SIZENAME>>dolle', d1);
//  s2 := d2.UpgradeFrom('Das<SIZE>>ist<SIZENAME>>', d1);
//  s2 := d2.UpgradeFrom('Das<SIZE>>ist<SIZENAME', d1);
//end;
//
//initialization
//begin
//  Test;
//end;
//
end.

