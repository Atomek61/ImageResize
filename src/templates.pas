unit templates;

{$mode delphi}
{$modeswitch advancedrecords}

//
// TEngine is a class to process a template string by replacing variable-references
// by their value. As a first step, the engine can "solve" the variables - that
// means it can solve dependencies between the variables.
//
// Variables are pairs of Name/Value (both strings). A value can contain a
// reference to another variable. Solving means to recursively reduce the
// dependencies until all are solved.
//
// I.e. FILENAME="img{SIZE}.{EXT}" refers to the other variables SIZE and EXT,
// which may refer to other variables.
//
// Usage:
//   Engine := TEngine.Create;
//   with Engine do begin
//     Add('INDEX', '1');
//     Add('SIZE', '1920');
//     Add('EXT', '.jpg');
//     Add('FILENAME', 'img{SIZE.ifmt(1,3)}.{EXT}');
//   end;
//   Engine.Solve;
//   result := Engine.Compile('My File {FILENAME} has a size of {SIZE} Pixels');
//

// \{(\w+)(?:\.((\w+)\(([\d,]*)\)))?\}
// "Hier kann {FILENAME.slice(3,4)} was sein"
// "Oder {INDEX.fmti(3,1)} hier"
// match(1) INDEX
// match(2) ifmt
// match(3) 3,1

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils, RegExpr, LCLproc;

const
  // The Pattern must be without surrounding delimiters - they will be added automatically
  DEFAULT_VARPATTERN = '(\w+)(?:\.(?:(\w+)\(([\w,]*)\)))?'; // VARIABLE.fn(p1,p2)

type

  TParamsArray = array of integer;

  { TDelimiters }

  TDelimiters = record
    Del1, Del2 :string;
    constructor Create(const Del1, Del2 :string);
    function toString :string;
    class function TryStrToDelimiters(const Value :string; out Delimiters :TDelimiters) :boolean; static;
    class function StrToDelimiters(const Value :string) :TDelimiters; static;
    class operator Equal(const Left, Right: TDelimiters): boolean;
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
  TEngine = class;


  { TEngine }


  TEngine = class
  public type

    TStaticFunction = function(const Value :string; const Params :TParamsArray) :string;
    TDynamicFunction = function(const VarName, Value, Params :string) :string of object;

    TStats = record
      DepsTotal :integer; // Is 0 after successfull solving all
      Solved :integer;
      Unknown :integer;
    end;

    { TStaticFunctionDef }

    TStaticFunctionDef = record
      Name :string;
      StaticFn :TStaticFunction;
      ParamsExpr :string;
      ParamCount :integer;
      function Call(const Value, Params :string) :string;
    end;

  private type

    TKeyVar = class
      Key :string;
      Value :string;
      Deps :array of TKeyVar;
      DepsCount :integer;  // Number of vars which do refer to this
    end;

    { TVarExpr }

    TVarExpr = record
      Name :string;
      Fn :string;
      Params :string;
      From :integer;
      Len :integer;
      function Top :integer;
    end;

    { TIterator }

    TIterator = record
      Engine :TEngine;
      Subject :string;
      IsMatch :boolean;
      constructor Create(Engine :TEngine; const Subject :string);
      function Next(out Expr :TVarExpr) :boolean;
    end;

  private
    FVarDict :TDictionary<string, TKeyVar>;
    FVars :TObjectList<TKeyVar>;
    FDelimiters :TDelimiters;
    FVarPattern :string;
    FVarExpr :TRegExpr;
    FStaticFns :TDictionary<string, TStaticFunctionDef>;
    FDynamicFunctions :TDictionary<string, TDynamicFunction>;
    FReplacementCount :integer;
    function GetCount: integer;
    function GetItem(const Key :string): string;
    function GetKey(Index :integer): string;
    function GetValue(Index :integer) :string; overload;
    procedure SetDelimiters(const AValue: TDelimiters);
    //function Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
    procedure SetItem(const Key :string; const Value: string);
    procedure CompileVarPattern;
    procedure SetVarPattern(AValue: string);
  public
    constructor Create(RegisterDefaultStaticFunctions :boolean = true);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key :string; const Value :string = '');
    procedure Load(const Key :string; const Value :string);
    procedure Reload(Index :integer; const Value :string);
    procedure RegisterStaticFunction(const Definition :TStaticFunctionDef);
    procedure RegisterDefaultStaticFunctions;
    procedure RegisterDynamicFunction(const Name :string; Fn :TDynamicFunction);
    function TrySolve(out Stats :TStats) :boolean;
    procedure Solve(out Stats :TStats);
    function Compile(const Subject :string) :string;
    function TryGetValue(const Key :string; out Value :string) :boolean;
    function GetValue(const Key :string) :string; overload;
    class function DecoratePattern(const Pattern :string; const Delimiters :TDelimiters) :string;
    class function Contains(const Template :string; const Names :array of string; const Delimiters :TDelimiters) :boolean;
    property Count :integer read GetCount;
    property Items[const Key :string] :string read GetItem write SetItem; default;
    property Keys[Index :integer] :string read GetKey;
    property Values[Index :integer] :string read GetValue;
    property Delimiters  :TDelimiters read FDelimiters write SetDelimiters;
    property VarPattern :string read FVarPattern write SetVarPattern;
    property ReplacementCount :integer read FReplacementCount;
  end;

// Checks against the default syntax VARNAME.fn(..)


implementation  // »SIZE«      «SIZE»

resourcestring
  SErrVarNotFoundFmt = 'Variable ''%s'' not found in VarEngine.';
  SErrInvalidDelimitersFmt = 'Invalid delimiters ''%s''.';
  SErrSolving = 'Cant solve valiables (circular dependencies?)';
  SErrInvalidFunctionFmt = 'Engine function %s not found';
  SErrInvalidStaticFunctionParamsFmt = 'Invalid parameter ''%s'' for engine function %s';
  SErrInvalidParamsFmt = 'invalid parameter ''%s''';
  SErrStaticFunctionFmt = 'Error in engine function %s - %s';
  SErrDynamicFunctionFmt = 'Error in engine dynamic function %s - %s';

const
  FN_LOWER_NAME = 'lower';
  FN_UPPER_NAME = 'upper';
  FN_IFMT_NAME  = 'ifmt';

type
  TIntegerArray = array of integer;

function FnLower(const Value :string; const Params :TParamsArray) :string;
begin
  result := LowerCase(Value);
end;

function FnUpper(const Value :string; const Params :TParamsArray) :string;
begin
  result := UpperCase(Value);
end;

// INDEX.ifmt(1,3)  0 => 001
function FnIfmt(const Value :string; const Params :TParamsArray) :string;
var
  IntValue :integer;
begin
  IntValue := StrToInt(Value);
  result := Format('%.*d', [Params[1], IntValue + Params[0]]);
end;

const
  DEFAULT_SOLVERFUNCTIONS :array[0..2] of TEngine.TStaticFunctionDef = (
    (Name: FN_LOWER_NAME; StaticFn: FnLower; ParamsExpr: ''; ParamCount: 0),
    (Name: FN_UPPER_NAME; StaticFn: FnUpper; ParamsExpr: ''; ParamCount: 0),
    (Name: FN_IFMT_NAME;  StaticFn: FnIFmt;  ParamsExpr: '(\d+),(\d+)'; ParamCount: 2)
  );

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

class operator TDelimiters.Equal(const Left, Right: TDelimiters): boolean;
begin
  result := (Left.Del1 = Right.Del1) and (Left.Del2 = Right.Del2);
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

{ TEngine }

function TEngine.GetCount: integer;
begin
  result := FVars.Count;
end;

function TEngine.GetItem(const Key :string): string;
begin
  result := FVarDict[Key].Value;
end;

function TEngine.GetKey(Index : integer): string;
begin
  result := FVars[Index].Key;
end;

procedure TEngine.SetItem(const Key :string; const Value :string);
var
  KeyVar :TKeyVar;
begin
  if FVarDict.TryGetValue(Key, KeyVar) then
    KeyVar.Value := Value
  else
    Add(Key, Value);
end;

procedure TEngine.CompileVarPattern;

  function escaped(const Delimiter :string) :string;
  var
    c :Char;
  begin
    result := '';
    for c in Delimiter do
      result := result + '\' + c;
  end;

begin
  FVarExpr.Free;
  FVarExpr := TRegExpr.Create(escaped(FDelimiters.Del1) + FVarPattern + escaped(FDelimiters.Del2));
end;

procedure TEngine.SetVarPattern(AValue: string);
begin
  if FVarPattern=AValue then Exit;
  FVarPattern:=AValue;
  CompileVarPattern;
end;

constructor TEngine.Create(RegisterDefaultStaticFunctions :boolean);
begin
  FVarDict := TDictionary<string, TKeyVar>.Create;
  FVars := TObjectList<TKeyVar>.Create;
  FDelimiters := CURLYBRACEDELIMITERS;
  FVarPattern := DEFAULT_VARPATTERN;
  FStaticFns := TDictionary<string, TStaticFunctionDef>.Create;
  if RegisterDefaultStaticFunctions then
    self.RegisterDefaultStaticFunctions;
  FDynamicFunctions := TDictionary<string, TDynamicFunction>.Create;
  CompileVarPattern;
end;

destructor TEngine.Destroy;
begin
  FVarDict.Free;
  FVars.Free;
  FVarExpr.Free;
  FStaticFns.Free;
  FDynamicFunctions.Free;
  inherited Destroy;
end;

procedure TEngine.Clear;
begin
  FVarDict.Clear;
  FVars.Clear;
end;

procedure TEngine.Add(const Key :string; const Value :string);
var
  KeyVar :TKeyVar;
begin
  KeyVar := TKeyVar.Create;
  try
    KeyVar.Key := Key;
    KeyVar.Value := Value;
    FVarDict.Add(Key, KeyVar);
  except
    KeyVar.Free;
    raise;
  end;
  FVars.Add(KeyVar);
end;

procedure TEngine.Load(const Key: string; const Value: string);
var
  KeyVar :TKeyVar;
begin
  if FVarDict.TryGetValue(Key, KeyVar) then
    KeyVar.Value := Value
  else
    Add(Key, Value);
end;

procedure TEngine.Reload(Index: integer; const Value: string);
begin
  FVars[Index].Value := Value;
end;

procedure TEngine.RegisterStaticFunction(const Definition :TStaticFunctionDef);
begin
  FStaticFns.Add(Definition.Name, Definition);
end;

procedure TEngine.RegisterDefaultStaticFunctions;
var
  Def :TStaticFunctionDef;
begin
  for Def in DEFAULT_SOLVERFUNCTIONS do
    RegisterStaticFunction(Def);
end;

procedure TEngine.RegisterDynamicFunction(const Name: string;
  Fn: TDynamicFunction);
begin
  FDynamicFunctions.Add(Name, Fn);
end;

function TEngine.TrySolve(out Stats: TStats): boolean;
var
  Iterator :TIterator;
  l, r, d :TKeyVar;
  Expr :TVarExpr;
  found :boolean;
  Dependencies :integer;
begin
  Stats.DepsTotal := 0;
  Stats.Solved := 0;
  Stats.Unknown := 0;
  if Count=0 then Exit(true);

  // Find all dependencies
  for l in FVars do begin
    Iterator := TIterator.Create(self, l.Value);
    while Iterator.Next(Expr) do begin
      if FVarDict.TryGetValue(Expr.Name, r) then begin
        // Check, if the dependency is counted only once
        found := false;
        for d in r.Deps do begin
          found := d=l;
          if found then break;
        end;
        if not found then begin
          inc(Stats.DepsTotal);
          inc(l.DepsCount);
          SetLength(r.Deps, Length(r.Deps)+1);
          r.Deps[High(r.Deps)] := l;
        end;
      end else
        inc(Stats.Unknown);
    end;
  end;

  // Solve all vars until no dependencies
  repeat
    Dependencies := Stats.DepsTotal;
    for l in FVars do if l.DepsCount=0 then begin
      for r in l.Deps do begin
        r.Value := Compile(r.Value);
        dec(r.DepsCount);
        dec(Stats.DepsTotal);
        inc(Stats.Solved);
      end;
      l.Deps := nil;
    end;
  until (stats.DepsTotal=0) or (Dependencies=Stats.DepsTotal);
  result := stats.DepsTotal=0;
end;

procedure TEngine.Solve(out Stats: TStats);
begin
  if not TrySolve(Stats) then
    raise Exception.Create(SErrSolving);
end;

function TEngine.Compile(const Subject: string): string;
var
  Iterator :TIterator;
  Expr :TVarExpr;
  KeyVar :TKeyVar;
  p :integer;
  Value :string;
  FnDef :TStaticFunctionDef;
  DynFn :TDynamicFunction;
begin
  FReplacementCount := 0;
  Iterator := TIterator.Create(self, Subject);
  result := '';
  p := 1;
  while Iterator.Next(Expr) do begin
    if FVarDict.TryGetValue(Expr.Name, KeyVar) then begin
      inc(FReplacementCount);
      // Ceck if function required
      if Expr.Fn<>'' then begin
        // 1st look for a dynamic function
        if FDynamicFunctions.TryGetValue(Expr.Fn, DynFn) then begin
          try
            Value := DynFn(Expr.Name, KeyVar.Value, Expr.Params);
          except on E :Exception do
            begin
              raise Exception.CreateFmt(SErrDynamicFunctionFmt, [Expr.Fn, E.Message]);
            end;
          end;
        end else begin
          // 2nd look for a static function
          if not FStaticFns.TryGetValue(Expr.Fn, FnDef) then
            raise Exception.CreateFmt(SErrInvalidFunctionFmt, [Expr.Fn]);
          try
            Value := FnDef.Call(KeyVar.Value, Expr.Params);
          except on E :Exception do
            begin
              raise Exception.CreateFmt(SErrStaticFunctionFmt, [Expr.Fn, E.Message]);
            end;
          end;
        end;
      end else
        // 3rd ignore replacement
        Value := KeyVar.Value;
      result := result + Copy(Subject, p, Expr.From-p) + Value;
    end else
      result := result + Copy(Subject, p, Expr.From-p+Expr.Len);
    p := Expr.From + Expr.Len;
  end;
  if FReplacementCount>0 then
    result := result + Copy(Subject, p, Length(Subject)-p+1)
  else
    Exit(Subject);
end;

function TEngine.TryGetValue(const Key: string; out Value: string): boolean;
var
  Rec :TKeyVar;
begin
  result := FVarDict.TryGetValue(Key, Rec);
  if result then
    Value := Rec.Value;
end;

function TEngine.GetValue(const Key: string): string;
begin
  if not TryGetValue(Key, result) then
    raise Exception.CreateFmt(SErrVarNotFoundFmt, [Key]);
end;

class function TEngine.DecoratePattern(const Pattern: string; const Delimiters: TDelimiters): string;

function escaped(const Delimiter :string) :string;
  var
    c :Char;
  begin
    result := '';
    for c in Delimiter do
      result := result + '\' + c;
  end;

begin
  result := escaped(Delimiters.Del1) + Pattern + escaped(Delimiters.Del2);
end;

class function TEngine.Contains(const Template: string; const Names: array of string; const Delimiters: TDelimiters): boolean;
var
  r :TRegExpr;
  Name :string;
begin
  r := TRegExpr.Create(DecoratePattern(DEFAULT_VARPATTERN, Delimiters));
  try
    r.InputString := Template;
    if r.Exec then repeat
      for Name in Names do
        if Name = r.Match[1] then Exit(True);
    until not r.ExecNext;
  finally
    r.Free;
  end;
  result := False;
end;

function TEngine.GetValue(Index: integer): string;
begin
  result := FVars[Index].Value;
end;

procedure TEngine.SetDelimiters(const AValue: TDelimiters);
begin
  if FDelimiters = AValue then Exit;
  FDelimiters:=AValue;
  CompileVarPattern;
end;

{ TEngine.TStaticFunctionDef }

function TEngine.TStaticFunctionDef.Call(const Value, Params: string): string;
var
  r :TRegExpr;
  ParamList :TParamsArray;
  i :integer;
  IntParam :integer;
begin
  ParamList := nil;
  if ParamCount<>0 then begin
    r := TRegExpr.Create(ParamsExpr);
    if not r.Exec(Params) then
      raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
    for i:=0 to ParamCount-1 do begin
      IntParam := StrToInt(r.Match[i+1]);
      SetLength(ParamList, Length(ParamList)+1);
      ParamList[High(ParamList)] := IntParam;
    end;
  end else
    if Params<>'' then
      raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
  result := StaticFn(Value, ParamList);
end;

{ TEngine.TVarExpr }

function TEngine.TVarExpr.Top: integer;
begin
  result := From+Len;
end;

{ TEngine.TIterator }

constructor TEngine.TIterator.Create(Engine: TEngine; const Subject :string);
begin
  self.Engine := Engine;
  self.Engine.FVarExpr.InputString := Subject;
  self.IsMatch := Engine.FVarExpr.Exec;
end;

function TEngine.TIterator.Next(out Expr :TVarExpr): boolean;
begin
  result := IsMatch;
  if result then with Expr do begin
    Name    := Engine.FVarExpr.Match[1];
    Fn      := Engine.FVarExpr.Match[2];
    Params  := Engine.FVarExpr.Match[3];
    From    := Engine.FVarExpr.MatchPos[0];
    Len     := Engine.FVarExpr.MatchLen[0];
    IsMatch := Engine.FVarExpr.ExecNext;
  end;
end;

//procedure Test;
//var
//  Engine :TEngine;
//  Stats :TEngine.TStats;
//  x :string;
//begin
//  Engine := TEngine.Create;
//  Engine.RegisterDefaultStaticFunctions;
//  try
//    Engine.Add('INDEX', '1');
//    Engine.Add('FILETITLE', 'dsc00123');
//    Engine.Add('FILEEXT', 'JPG');
//    Engine.Add('FILENAME', '{FILETITLE.upper()}_{INDEX.ifmt(1,3)}.{FILEEXT.lower()}');
//
//    Engine.TrySolve(Stats);
//
//    x := Engine.Compile('Ab {INDEX} ist {FILENAME}.');
//
//  finally
//    Engine.Free;
//  end;
//end;
//
initialization
begin
//  Test;
end;

end.

