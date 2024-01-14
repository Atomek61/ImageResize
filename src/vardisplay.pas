unit VarDisplay;

{$mode delphi}

interface

uses
  Classes, SysUtils, DateTimeUtils, Logging;

type

  { TVar }

  TFlag = (tfValid);
  TFlags = set of TFlag;

  TVarClass = class of TVar;

  TVar = class
  private
    FFlags :TFlags;
    function GetValid: boolean;
    procedure SetValid(AValue: boolean);
    function GetAsText: string;
    procedure SetAsText(const AValue: string); virtual;
  protected
    procedure Validate;
    function GetDisplay: string; virtual;
    procedure SetDisplay(const Value :string); virtual;
    function TrySetText(const AValue :string) :boolean; virtual; abstract;
    function GetText :string; virtual; abstract;
    procedure SetDefault; virtual; abstract;
    procedure SetDisplayFormat(AValue: string); virtual; abstract;
    function GetDisplayFormat: string; virtual; abstract;
  public
    constructor Create; virtual; overload;
    class function Create(const TypeId :string) :TVar; overload;
    class function TypeId :string;
    class procedure Register(Value :TVarClass);
    procedure Invalidate;
    property Valid :boolean read GetValid write SetValid;
    property Text :string read GetAsText write SetAsText;
    property Display :string read GetDisplay write SetDisplay;
    property DisplayFormat :string read GetDisplayFormat write SetDisplayFormat;
  end;

  { TBooleanVar }

  TBooleanVar = class(TVar)
  protected
    function GetText: string; override;
    function TrySetText(const AValue :string) :boolean; override;
    procedure SetDefault; override;
    function GetDisplay: string; override;
  public
    Value :Boolean;
    FalseText, TrueText :string;
    FalseDisplay, TrueDisplay :string;
    constructor Create; override;
  end;

  { TIntegerVar }

  TIntegerVar = class(TVar)
  protected
    function GetText: string; override;
    function TrySetText(const AValue :string) :boolean; override;
    procedure SetDefault; override;
  public
    Value :Int64;
  end;

  { TFloatVar }

  TFloatVar = class(TVar)
  protected
    function GetText: string; override;
    function TrySetText(const AValue :string) :boolean; override;
    procedure SetDefault; override;
    function GetDisplay: string; override;
  public
    Value :Double;
    TextFormat :string;
    DisplayFormat :string;
    constructor Create; override;
  end;

  { TTimestampVar }

  TTimestampVar = class(TVar)
  protected
    function GetText :string; override;
    function TrySetText(const AValue :string) :boolean; override;
    procedure SetDefault; override;
    function GetDisplay: string; override;
  public
    Value :TDateTime;
    TextFormat :string;
    DisplayFormat :string;
    constructor Create; override;
  end;

  { TStringVar }

  TStringVar = class(TVar)
    Value :string;
  protected
    function GetText: string; override;
    function TrySetText(const AValue :string) :boolean; override;
    procedure SetDefault; override;
  end;

  { TByteSizeVar }

  TByteSizeVar = class(TIntegerVar)
  protected
    function GetDisplay: string; override;
  end;

  resourcestring
    SErrVarClassNotRegisteredFmt = 'Variable class ''%s'' not registered.';
    SErrInvalidVarStringFmt = 'Cant convert ''%s'' to variable of type %s.';

implementation

uses
  Generics.Collections;

type
  TVarClasses = TDictionary<string, TVarClass>;

var
  VarClasses :TVarClasses;

{ TVar }

class function TVar.Create(const TypeId: string): TVar;
var
  VarClass :TVarClass;
begin
  if not VarClasses.TryGetValue('T'+TypeId+'Var', VarClass) then
    Exception.CreateFmt(SErrVarClassNotRegisteredFmt, [TypeId]);
  result := VarClass.Create;
end;

function TVar.GetValid: boolean;
begin
  result := tfValid in FFlags;
end;

procedure TVar.SetValid(AValue: boolean);
begin
  if AValue=Valid then Exit;
  if not AValue then begin
    SetDefault;
    Exclude(FFlags, tfValid);
  end;
end;

procedure TVar.Validate;
begin
  include(FFlags, tfValid);
end;

function TVar.GetDisplay: string;
begin
  result := Text;
end;

procedure TVar.SetDisplay(const Value: string);
begin
  Text := Value;
end;

constructor TVar.Create;
begin
  Invalidate;
end;

function TVar.GetAsText: string;
begin
  if Valid then
    result := GetText
  else
    result := '';
end;

procedure TVar.SetAsText(const AValue: string);
begin
  if not TrySetText(AValue) then
    EConvertError.CreateFmt(SErrInvalidVarStringFmt, [AValue, TypeId]);
  Validate;
end;

class function TVar.TypeId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-4);
end;

class procedure TVar.Register(Value: TVarClass);
begin
  VarClasses.Add(Value.ClassName, Value);
end;

procedure TVar.Invalidate;
begin
  Exclude(FFlags, tfValid);
  SetDefault;
end;

{ TBooleanVar }

function TBooleanVar.GetText: string;
begin
  if Value then
    result := TrueText
  else
    result := FalseText;
end;

function TBooleanVar.TrySetText(const AValue: string): boolean;
begin
  if SameText(TrueText, AValue) then
    Value := true
  else if SameText(FalseText, AValue) then
    Value := false
  else
    Exit(false);
  result := false;
end;

procedure TBooleanVar.SetDefault;
begin
  Value := False;
end;

function TBooleanVar.GetDisplay: string;
begin
  if Value then
    result := TrueDisplay
  else
    result := FalseDisplay;
end;

constructor TBooleanVar.Create;
begin
  inherited;
  FalseText := '0';
  TrueText := '1';
  FalseDisplay := 'False';
  TrueDisplay := 'True';
end;

{ TIntegerVar }

function TIntegerVar.GetText: string;
begin
  result := IntToStr(Value)
end;

function TIntegerVar.TrySetText(const AValue: string): boolean;
begin
  result := TryStrToInt64(AValue, Value);
end;

procedure TIntegerVar.SetDefault;
begin
  Value := 0;
end;

{ TFloatVar }

function TFloatVar.GetText: string;
begin
  result := Format(TextFormat, [Value])
end;

function TFloatVar.TrySetText(const AValue: string): boolean;
begin
  result := TryStrToFloat(AValue, Value);
end;

procedure TFloatVar.SetDefault;
begin
  Value := 0.0;
end;

function TFloatVar.GetDisplay: string;
begin
  result := Format(DisplayFormat, [Value])
end;

constructor TFloatVar.Create;
begin
  inherited;
  TextFormat := '%g';
  DisplayFormat := '%g';
end;

{ TTimestampVar }

constructor TTimestampVar.Create;
begin
  inherited;
  TextFormat := ISODATETIMEFMT;
  TextFormat := 'YYYY-MM-DD HH:NN';
end;

function TTimestampVar.GetText: string;
begin
  result := DateTimeToStr(Value, TextFormat);
end;

function TTimestampVar.TrySetText(const AValue: string): boolean;
begin
  result := TryStrToDateTime(AValue, Value);
end;

procedure TTimestampVar.SetDefault;
begin
  Value := 0.0;
end;

function TTimestampVar.GetDisplay: string;
begin
  result := DateTimeToStr(Value, DisplayFormat);
end;

{ TStringVar }

function TStringVar.GetText: string;
begin
  result := Value;
end;

function TStringVar.TrySetText(const AValue: string): boolean;
begin
  Value := AValue;
  result := true;
end;

procedure TStringVar.SetDefault;
begin
  Value := '';
end;

{ TByteSizeVar }

type
  TSizeInfo = record
    Limit :Int64;
    Fmt :string;
    Divisor :Double;
  end;

const
  SIZEINFOS :array[0..5] of TSizeInfo = (
    (Limit:        10000; Fmt: '%.1f kB'; Divisor:         1000.0;),
    (Limit:      1000000; Fmt: '%.0f kB'; Divisor:         1000.0;),
    (Limit:     10000000; Fmt: '%.1f MB'; Divisor:      1000000.0;),
    (Limit:   1000000000; Fmt: '%.0f MB'; Divisor:      1000000.0;),
    (Limit:  10000000000; Fmt: '%.1f GB'; Divisor:   1000000000.0;),
    (Limit:1000000000000; Fmt: '%.0f GB'; Divisor:   1000000000.0;)
  );

function TByteSizeVar.GetDisplay: string;
var
  Info :TSizeInfo;
begin
  if Value<1000 then Exit(inherited);
  for Info in SIZEINFOS do with Info do
    if Value<Limit then
      Exit(Format(Fmt, [Value/Divisor]));
  result := Format('%.1fTB', [Value/1E12])
end;

procedure Test;
var
  v :TVar;
begin
//  Log(Format('%.0 kBf', [123400/1000]));
  v := TVar.Create('ByteSize');
  v.Text := '123400';
  Log(v.Display);
end;

initialization
begin
  VarClasses := TVarClasses.Create;
  TVar.Register(TBooleanVar);
  TVar.Register(TIntegerVar);
  TVar.Register(TFloatVar);
  TVar.Register(TStringVar);
  TVar.Register(TTimestampVar);
  TVar.Register(TByteSizeVar);
  Test
end;

finalization
begin
  VarClasses.Free;
end;

end.

