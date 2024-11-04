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

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils, RegExpr, LCLproc;

const
  // The Pattern must be without surrounding delimiters - they will be added automatically
//  VARPATTERN = '^(\w+)(?:\.(?:(\w+)\(([\w,:\-]*)\)))?$'; // VARIABLE.fn(p1,p2)
  EXPPATTERN = '^(\w+)((?:\.(?:\w+)\((?:[\w\d\-,:]*)\))*)$';
  FNCPATTERN = '\.(\w+)\(([\w\d\-,:]*)\)';

  IFMT_PARAMS_PATTERN   = '(\d+)(?:,(\d+))';
  SLICE_PARAMS_PATTERN  = '(\d+)*:((?:-?\d+)*)';
  //ADD_PARAMS_PATTERN    = '(\d+)';
  //FMT_PARAMS_PATTERN    = '(\d+)';

type

  TIntegerArray = array of integer;

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

    TTemplateFunction = function(const Value :string; const Params :string) :string of object;

    TStats = record
      DepsTotal :integer; // Is 0 after successfull solving all
      Solved :integer;
      Unknown :integer;
    end;

    TFunctionDef = record
      Name :string;
      Fn :TTemplateFunction;
    end;

  private type

    TKeyVar = class
      Key :string;
      Value :string;
      Deps :array of TKeyVar;
      DepsCount :integer;  // Number of vars which do refer to this
    end;

    { TExpression }

    TExpression = record
      VarName :string;
      FnCalls :string;
    end;

    TFnCall = record
      FnName :string;
      Params :string;
    end;
    TFnCalls = TArray<TFnCall>;

    { TIterator }

    TIterator = record
      Delimiters :TDelimiters;
      Cursor1 :integer;
      Cursor2 :integer;
      Subject :string;
      From :integer;
      Len :integer;
      IsMatch :boolean;
      constructor Create(const Subject :string; const Delimiters :TDelimiters);
      function Next(out Expression :string) :boolean;
    end;

  private
    FVarDict :TDictionary<string, TKeyVar>;
    FVars :TObjectList<TKeyVar>;
    FDelimiters :TDelimiters;
    FExpRegExpr :TRegExpr;
    FFnCallsRegExpr :TRegExpr;
    FFunctions :TDictionary<string, TTemplateFunction>;
    FReplacementCount :integer;
    function GetCount: integer;
    function GetItem(const Key :string): string;
    function GetKey(Index :integer): string;
    function GetValue(Index :integer) :string; overload;
    procedure SetDelimiters(const AValue: TDelimiters);
    //function Next(const Txt :string; var Iterator :TIterator; out Key :string) :boolean;
    procedure SetItem(const Key :string; const Value: string);
    //procedure CompileVarPattern;
    //procedure SetVarPattern(AValue: string);
    class function ParseName(const Expression :string) :string;
    function ParseExpression(const Expression :string) :TExpression;
    function ParseFnCalls(const FnCalls :string) :TArray<TFnCall>;
    function FnLower(const Value, Params :string) :string;
    function FnUpper(const Value, Params :string) :string;
    function FnIFmt(const Value, Params :string) :string;
    function FnSlice(const Value, Params :string) :string;
    function FnAdd(const Value, Params :string) :string;
    function FnFmt(const Value, Params :string) :string;
    function FnTitle(const Value, Params :string) :string;
    function FnExt(const Value, Params :string) :string;
    function FnDext(const Value, Params :string) :string;
    function FnNumIn(const Value, Params :string) :string;
  public
    Stats :TStats;
    constructor Create(RegisterDefaultFunctions :boolean = true);
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const Key :string; const Value :string = '');
    procedure Load(const Key :string; const Value :string);
    procedure Reload(Index :integer; const Value :string);
    procedure RegisterFunction(const Name :string; Fn :TTemplateFunction);
    function TrySolve :boolean;
    procedure Solve;
    function Compile(const Subject :string) :string;
    function TryGetValue(const Key :string; out Value :string) :boolean;
    function GetValue(const Key :string) :string; overload;
//    class function DecoratePattern(const Pattern :string; const Delimiters :TDelimiters) :string;
    class function ContainsOneOf(const Template :string; const Names :array of string; const Delimiters :TDelimiters) :boolean;
    property Count :integer read GetCount;
    property Items[const Key :string] :string read GetItem write SetItem; default;
    property Keys[Index :integer] :string read GetKey;
    property Values[Index :integer] :string read GetValue;
    property Delimiters  :TDelimiters read FDelimiters write SetDelimiters;
//    property VarPattern :string read FVarPattern write SetVarPattern;
    property ReplacementCount :integer read FReplacementCount;
  end;

// Checks against the default syntax VARNAME.fn(..)


resourcestring
  SErrInvalidExpressionFmt      = 'Invalid template expression "%s"';
  SErrVarNotFoundFmt            = 'Variable "%s" not found in template engine.';
  SErrInvalidDelimitersFmt      = 'Invalid delimiters "%s".';
  SErrSolving                   = 'Cant solve variables (circular dependencies?)';
  SErrInvalidParamsFmt          = 'invalid parameter "%s"';
  SErrInvalidNumberOfParamsFmt  = 'invalid number of parameters (%d expected)';
  SErrInvalidNoParamsExpected   = 'no parameter expected';
  SErrTemplateFunctionNotFoundFmt = 'Function "%s" not found';
  SErrDynamicFunctionFmt        = 'Error in template engine function %s - %s';
  SErrInvalidParamNotInt        = 'integer expected';

implementation

function IsWordChar(c: Char): Boolean;
begin
  Result := c in ['A'..'Z', 'a'..'z', '0'..'9', '_'];
end;

function Split(const Input: string; const Separators: array of Char): TArray<string>;
var
  s, e, i: Integer;
  IsSeparator: Boolean;
begin
  Result := nil;
  s := 1;
  while s <= Length(Input) do begin
    e := s;
    while e <= Length(Input) do begin
      IsSeparator := False;
      for i := Low(Separators) to High(Separators) do begin
        if Input[e] = Separators[i] then begin
          IsSeparator := True;
          Break;
        end;
      end;
      if IsSeparator then
        Break;
      Inc(e);
    end;
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)] := Copy(Input, s, e - s);
    s := e + 1;
  end;
end;

function StrToIntegerArray(const Value: string; const Delimiters :array of char): TIntegerArray;
var
  s :TArray<string>;
  i, n :integer;
begin
  result := nil;
  s := Split(Value, Delimiters);
  for i:=0 to High(s) do
    if not TryStrToInt(s[i], n) then
      raise Exception.Create(SErrInvalidParamNotInt)
    else begin
      SetLength(result, Length(result)+1);
      result[High(result)] := n;
    end;
end;

function TEngine.FnLower(const Value, Params: string): string;
begin
  if Params<>'' then
    raise Exception.CreateFmt(SErrInvalidNumberOfParamsFmt, [0]);
  result := LowerCase(Value);
end;

function TEngine.FnUpper(const Value, Params: string): string;
begin
  if Params<>'' then
    raise Exception.CreateFmt(SErrInvalidNumberOfParamsFmt, [0]);
  result := UpperCase(Value);
end;

// INDEX.ifmt(1,3)  0 => 001
function TEngine.FnIFmt(const Value, Params: string): string;
var
  IntParams :TIntegerArray;
  Offset, Digits :integer;
  IntValue :integer;
  r :TRegExpr;
begin
  if not TryStrToInt(Value, IntValue) then
    raise Exception.Create(SErrInvalidParamNotInt);
  r := TRegExpr.Create(IFMT_PARAMS_PATTERN);
  if not r.Exec(Params) then
    raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
  IntParams := StrToIntegerArray(Params, [',']);
  Offset := IntParams[0];
  if Length(IntParams)=2 then Digits := IntParams[1] else Digits := 0;
  if Digits=0 then
    result := IntToStr(IntValue+Offset)
  else
    result := Format('%*.*d', [Digits, Digits, IntValue + Offset]);
end;

function TEngine.FnSlice(const Value, Params: string): string;
var
  r :TRegExpr;
  p0, p1 :integer;
begin
  r := TRegExpr.Create(SLICE_PARAMS_PATTERN);
  if not r.Exec(Params) then
    raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
  if r.Match[1]='' then
    p0 := 1
  else
    p0 := StrToInt(r.Match[1]);
  if r.Match[2]='' then
    p1 := Length(Value)
  else begin
    p1 := StrToInt(r.Match[2]);
    if p1<0 then
      p1 := Length(Value)+p1;
  end;
  result := Copy(Value, p0, p1-p0+1);
end;

function TEngine.FnAdd(const Value, Params: string): string;
var
  Int :integer;
  Offset :integer;
begin
  if not TryStrToInt(Value, Int) then
    raise Exception.Create(SErrInvalidParamNotInt);
  if not TryStrToInt(Params, Offset) then
    raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
  result := IntToStr(Int + Offset);
end;

function TEngine.FnFmt(const Value, Params: string): string;
var
  Int, Digits :integer;
begin
  if not TryStrToInt(Value, Int) then
    raise Exception.Create(SErrInvalidParamNotInt);
  if not TryStrToInt(Params, Digits) then
    raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
  result := Format('%*.*d', [Digits, Digits, Int]);
end;

function TEngine.FnTitle(const Value, Params: string): string;
var
  i :integer;
begin
  if Params<>'' then
    raise Exception.Create(SErrInvalidNoParamsExpected);
  i := Length(Value);
  while true do begin
    if i=0 then Exit(Value);
    if Value[i]='.' then Exit(Copy(Value, 1, i-1));
    dec(i);
  end;
end;

function TEngine.FnExt(const Value, Params: string): string;
var
  i :integer;
begin
  if Params<>'' then
    raise Exception.Create(SErrInvalidNoParamsExpected);
  i := Length(Value);
  while true do begin
    if i=0 then Exit('');
    if Value[i]='.' then Exit(Copy(Value, i+1, Length(Value)-i));
    dec(i);
  end;
end;

function TEngine.FnDext(const Value, Params: string): string;
var
  i :integer;
begin
  if Params<>'' then
    raise Exception.Create(SErrInvalidNoParamsExpected);
  i := Length(Value);
  while true do begin
    if i=0 then Exit('');
    if Value[i]='.' then Exit(Copy(Value, i, Length(Value)-i+1));
    dec(i);
  end;
end;

function TEngine.FnNumIn(const Value, Params: string): string;
var
  r :TRegExpr;
begin
  if Params<>'' then
    raise Exception.Create(SErrInvalidNoParamsExpected);
  r := TRegExpr.Create('\d+');
  try
    if r.Exec(Value) then
      result := r.Match[0]
    else
      result := '';
  finally
    r.Free;
  end;
end;

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

constructor TEngine.Create(RegisterDefaultFunctions :boolean);
begin
  FVarDict := TDictionary<string, TKeyVar>.Create;
  FVars := TObjectList<TKeyVar>.Create;
  FDelimiters := CURLYBRACEDELIMITERS;
  FExpRegExpr := TRegExpr.Create(EXPPATTERN);
  FFnCallsRegExpr := TRegExpr.Create(FNCPATTERN);
  FFunctions := TDictionary<string, TTemplateFunction>.Create;
  if RegisterDefaultFunctions then begin
    RegisterFunction('lower', FnLower);
    RegisterFunction('upper', FnUpper);
    RegisterFunction('ifmt', FnIFmt);
    RegisterFunction('slice', FnSlice);
    RegisterFunction('add', FnAdd);
    RegisterFunction('fmt', FnFmt);
    RegisterFunction('title', FnTitle);
    RegisterFunction('ext', FnExt);
    RegisterFunction('dext', FnDext);
    RegisterFunction('numin', FnNumIn);
  end;
end;

destructor TEngine.Destroy;
begin
  FVarDict.Free;
  FVars.Free;
  FExpRegExpr.Free;
  FFnCallsRegExpr.Free;
  FFunctions.Free;
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

procedure TEngine.RegisterFunction(const Name :string; Fn :TTemplateFunction);
begin
  FFunctions.AddOrSetValue(Name, Fn);
end;

function TEngine.TrySolve: boolean;
var
  Iterator :TIterator;
  l, r, d :TKeyVar;
  Expression :string;
  found :boolean;
  Dependencies :integer;
  Name :string;
begin
  Stats.DepsTotal := 0;
  Stats.Solved := 0;
  Stats.Unknown := 0;
  if Count=0 then Exit(true);

  // Find all dependencies
  for l in FVars do begin
    Iterator := TIterator.Create(l.Value, FDelimiters);
    while Iterator.Next(Expression) do begin
      Name := ParseName(Expression);
      if not FVarDict.TryGetValue(Name, r) then
        raise Exception.CreateFmt(SErrVarNotFoundFmt, [Name]);
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

procedure TEngine.Solve;
begin
  if not TrySolve then
    raise Exception.Create(SErrSolving);
end;

function TEngine.Compile(const Subject: string): string;
var
  Iterator :TIterator;
  ExprStr :string;
  Expr :TExpression;
  KeyVar :TKeyVar;
  Value :string;
  FnCall :TFnCall;
  FnCalls :TFnCalls;
  Fn :TTemplateFunction;
  Cursor :integer;
begin
  FReplacementCount := 0;
  result := '';
  Cursor := 1;
  Iterator := TIterator.Create(Subject, FDelimiters);
  while Iterator.Next(ExprStr) do begin
    // Parse Expression
    Expr := ParseExpression(ExprStr);
    if not FVarDict.TryGetValue(Expr.VarName, KeyVar) then
      raise Exception.CreateFmt(SErrVarNotFoundFmt, [Expr.VarName]);
    Value := KeyVar.Value;
    if Expr.FnCalls<>'' then begin
      FnCalls := ParseFnCalls(Expr.FnCalls);
      for FnCall in FnCalls do begin
        if not FFunctions.TryGetValue(FnCall.FnName, Fn) then
          raise Exception.CreateFmt(SErrTemplateFunctionNotFoundFmt, [FnCall.FnName]);
        try
          Value := Fn(Value, FnCall.Params);
        except on E :Exception do
          begin
            raise Exception.CreateFmt(SErrDynamicFunctionFmt, [FnCall.FnName, E.Message]);
          end;
        end;
      end;
    end;
    result := result + Copy(Subject, Cursor, Iterator.From - Cursor) + Value;
    Cursor := Iterator.From + Iterator.Len;
    inc(FReplacementCount);
  end;
  if FReplacementCount>0 then
    result := result + Copy(Subject, Cursor, Length(Subject)-Cursor+1)
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

//class function TEngine.DecoratePattern(const Pattern: string; const Delimiters: TDelimiters): string;

//function escaped(const Delimiter :string) :string;
//  var
//    c :Char;
//  begin
//    result := '';
//    for c in Delimiter do
//      result := result + '\' + c;
//  end;
//
//begin
//  result := escaped(Delimiters.Del1) + Pattern + escaped(Delimiters.Del2);
//end;
//
class function TEngine.ContainsOneOf(const Template: string; const Names: array of string; const Delimiters: TDelimiters): boolean;
var
  VarName, Name :string;
  Expression :string;
  Iterator :TIterator;
begin
  Iterator := TIterator.Create(Template, Delimiters);
  while Iterator.Next(Expression) do begin
    VarName := ParseName(Expression);
    for Name in Names do
      if Name = VarName then Exit(True);
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
end;

{ TEngine.TStaticFunctionDef }

//function TEngine.TStaticFunctionDef.Call(const Value, Params: string): string;
//var
//  r :TRegExpr;
//begin
//  if ParamCount<>0 then begin
//    r := TRegExpr.Create('^'+ParamsExpr+'$');
//    if not r.Exec(Params) then
//      raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
//  end else
//    if Params<>'' then
//      raise Exception.CreateFmt(SErrInvalidParamsFmt, [Params]);
//  result := StaticFn(Value, ParamCount, Params);
//end;
//
{ TEngine.TIterator }

constructor TEngine.TIterator.Create(const Subject :string; const Delimiters :TDelimiters);
begin
  self.Delimiters := Delimiters;
  self.Subject := Subject;
  Cursor1 := Pos(Delimiters.Del1, Subject);
  IsMatch := Cursor1>0;
  if IsMatch then begin
    Cursor2 := Pos(Delimiters.Del2, Subject, Cursor1+Length(Delimiters.Del1));
    IsMatch := Cursor2>0;
  end;
end;

function TEngine.TIterator.Next(out Expression :string): boolean;
begin
  result := IsMatch;
  if result then begin
    From := Cursor1;
    Len := Cursor2 - Cursor1 + Length(Delimiters.Del2);
    Expression := Copy(Subject, Cursor1 + Length(Delimiters.Del1), Cursor2 - Cursor1 - Length(Delimiters.Del1));
    Cursor1 := Pos(Delimiters.Del1, Subject, Cursor2 + Length(Delimiters.Del2));
    IsMatch := Cursor1>0;
    if IsMatch then begin
      Cursor2 := Pos(Delimiters.Del2, Subject, Cursor1+Length(Delimiters.Del1));
      IsMatch := Cursor2>0;
    end;
  end;
end;

class function TEngine.ParseName(const Expression: string): string;
var
  p :integer;
begin
  p :=1;
  if Length(Expression)>0 then while true do begin
    if (p>Length(Expression)) or not IsWordChar(Expression[p]) then
      Exit(Copy(Expression, 1, p-1));
    inc(p);
  end;
  raise Exception.CreateFmt(SErrInvalidExpressionFmt, [Expression]);
end;

function TEngine.ParseExpression(const Expression: string): TExpression;
begin
  if not FExpRegExpr.Exec(Expression) then
    raise Exception.CreateFmt(SErrInvalidExpressionFmt, [Expression]);
  result.VarName := FExpRegExpr.Match[1];
  result.FnCalls := FExpRegExpr.Match[2];
end;

function TEngine.ParseFnCalls(const FnCalls: string): TArray<TFnCall>;
var
  FnCall :TFnCall;
begin
  result := nil;
  if not FFnCallsRegExpr.Exec(FnCalls) then
    raise Exception.CreateFmt(SErrInvalidExpressionFmt, [FnCalls]);
  repeat
    FnCall.FnName := FFnCallsRegExpr.Match[1];
    FnCall.Params := FFnCallsRegExpr.Match[2];
    SetLength(result, Length(result)+1);
    result[High(result)] := FnCall;
  until not FFnCallsRegExpr.ExecNext;

end;

//procedure Test;
//var
//  Engine :TEngine;
//  Stats :TEngine.TStats;
//  x :string;
//begin
//  Engine := TEngine.Create;
//  try
//    Engine.Add('INDEX', '12345');
//    Engine.Add('FILENAME', 'dsc00123.jpg');
//    //Engine.Add('FILEEXT', 'JPG');
//    //Engine.Add('FILENAME', '{FILETITLE.upper()}_{INDEX.ifmt(1,3)}.{FILEEXT.lower()}');
//
//    Engine.TrySolve(Stats);
//    x := Engine.Compile('abc{FILENAME.lower()}xyz');
//    x := Engine.Compile('abc{FILENAME.upper()}xyz');
//    x := Engine.Compile('abc{INDEX.ifmt(10,8)}xyz');
//
//    x := Engine.Compile('abc{INDEX.slice(2:-1)}xyz{FILENAME.lower()}');
//
//  finally
//    Engine.Free;
//  end;
//end;
//

//procedure Test;
//var
//  x :TArray<string>;
//begin
//  x := Split('1,2,3', [',', ':']);
//  x := Split(',2,3', [',', ':']);
//  x := Split('1,2,', [',', ':']);
//end;
//
initialization
begin
//  Test;
end;

end.

