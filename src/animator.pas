unit animator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls;

type
  TGradient = (agLinear, agEaseIn, agEaseOut, agEaseInAndOut);
  TGradientFunction = function(x :double) :double;

const
  DEFAULT_RESOLUTION = 50;
  DEFAULT_MIN = 0.0;
  DEFAULT_MAX = 1.0;
  DEFAULT_GRADIENT = agLinear;
  DEFAULT_DURATION = 0.4;

type

  { TAnimator }

  TAnimationEvent = procedure(Sender :TObject; Value :double) of object;

  TAnimator = class
  private
    FOnAnimate :TAnimationEvent;
    FTimer :TTimer;
    FDuration :TTime;
    FResolution :integer;
    FGradient :TGradient;
    FGradientFunction :TGradientFunction;
    FMin :double;
    FMax :double;
    FT0 :TDateTime;
    function GetEnabled: boolean;
    procedure OnTimer(Sender :TObject);
    procedure SetDuration(AValue: TTime);
    procedure SetEnabled(AValue: boolean);
    procedure SetGradient(AValue: TGradient);
    procedure SetMax(AValue: double);
    procedure SetMin(AValue: double);
    procedure SetResolution(AValue: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Enabled :boolean read GetEnabled write SetEnabled;
    property Min :double read FMin write SetMin;
    property Max :double read FMax write SetMax;
    property Resolution :integer read FResolution write SetResolution;
    property Gradient :TGradient read FGradient write SetGradient;
    property Duration :TTime read FDuration write SetDuration;
    property OnAnimate :TAnimationEvent read FOnAnimate write FOnAnimate;
  end;

implementation

const PI2 = PI/2.0;

function Linear(x :double) :double;
begin
  result := x;
end;

function EaseIn(x :double) :double;
begin
  result := sin(x*PI2-PI2)+1.0;
end;

function EaseOut(x :double) :double;
begin
  result := sin(x*PI2);
end;

function EaseInAndOut(x :double) :double;
begin
  result := sin(x*PI-PI2)/2.0+0.5;
end;

const
  GRADIENTFUNCTIONS :array[TGradient] of TGradientFunction = (@Linear, @EaseIn, @EaseOut, @EaseInAndOut);

{ TAnimator }

constructor TAnimator.Create;
begin
  FMin := DEFAULT_MIN;
  FMax := DEFAULT_MAX;
  FResolution := DEFAULT_RESOLUTION;
  FDuration := DEFAULT_DURATION;
  FGradient := agLinear;
  FGradientFunction := @Linear;
end;

destructor TAnimator.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

function TAnimator.GetEnabled: boolean;
begin
  result := Assigned(FTimer);
end;

procedure TAnimator.OnTimer(Sender: TObject);
var
  T1 :TDateTime;
  x, y :double;
begin
  T1 := Now;
  x := (T1 - FT0)*SECSPERDAY/FDuration;
  if x>=1.0 then begin
    x := 1.0;
    FreeAndNil(FTimer);
  end;
  y := FMin+(FMax-FMin)*FGradientFunction(x);
  if Assigned(FOnAnimate) then
    FOnAnimate(self, y);
end;

procedure TAnimator.SetDuration(AValue: TTime);
begin
  if FDuration<FResolution*MSECSPERDAY then
    FDuration := FResolution*MSECSPERDAY;
  if FDuration=AValue then Exit;
  FDuration:=AValue;
end;

procedure TAnimator.SetEnabled(AValue: boolean);
begin
  if AValue=Enabled then Exit;
  if AValue then begin
    FTimer := TTimer.Create(nil);
    FTimer.Interval := FResolution;
    FTimer.OnTimer := @OnTimer;
    FTimer.Enabled := true;
    FT0 := Now;
  end else begin
    FreeAndNil(FTimer);
  end;
end;

procedure TAnimator.SetGradient(AValue: TGradient);
begin
  if FGradient=AValue then Exit;
  FGradient:=AValue;
  FGradientFunction := GRADIENTFUNCTIONS[FGradient];
end;

procedure TAnimator.SetMax(AValue: double);
begin
  if FMax=AValue then Exit;
  FMax:=AValue;
end;

procedure TAnimator.SetMin(AValue: double);
begin
  if FMin=AValue then Exit;
  FMin:=AValue;
end;

procedure TAnimator.SetResolution(AValue: integer);
begin
  if AValue<1 then AValue := 1;
  if FResolution=AValue then Exit;
  FResolution:=AValue;
end;

procedure TAnimator.Start;
begin
  Enabled := true;
end;

procedure TAnimator.Stop;
begin
  Enabled := false;
end;

end.

