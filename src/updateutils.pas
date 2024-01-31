unit updateutils;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils;

type

  { TVersion }

  TVersion = record
  private
    function GetAsString: string;
    function GetAsUInt32: UInt32;
    procedure SetAsString(const AValue: string);
    procedure SetUint32(AValue: UInt32);
  public
//    class operator >(const Op1, Op2 :TVersion) :boolean;
    constructor Create(const str :string);
    property AsString :string read GetAsString write SetAsString;
    property AsUInt32 :UInt32 read GetAsUInt32 write SetUint32;
    case integer of
    1: (
        Major :UInt32;
        Minor :UInt32;
        Patch :UInt32;
        Build :UInt32;
      );
    2:
      (
        AsArray :array[0..3] of UInt32;
      );
  end;

  { TVersionManifest }

  TVersionManifest = record
  private
    function GetAsDateTime: TDateTime;
    function GetAsString: string;
//    function GetVersion: TVersion;
    procedure SetAsString(const AValue: string);
  public
    App :string;
    Version :string;
    Date :string;
    Hint :string;
    constructor Create(const AApp, AVersion, ADate, AHint :string);
    property AsString :string read GetAsString write SetAsString;
    property AsDateTime :TDateTime read GetAsDateTime;
//    property AsVersion :TVersion read GetVersion;
  end;

implementation

uses
  datetimeutils;

const
  CRLF = #13#10;

//function NextKeyValue(const str :string; var p :integer; out Key, Value :string) :boolean;
//var
//  p0 :integer;
//begin
//  p0 := p + 1;
//  while p0<=Length(str) do begin
//  end;
//end;
//
{ TVersionManifest }

function TVersionManifest.GetAsDateTime: TDateTime;
begin
  result := StrToDate(Date);
end;

function TVersionManifest.GetAsString: string;
begin
  result := 'App='+App+CRLF+'Version='+Version+CRLF+'Date='+Date+CRLF+'Hint='+Hint+CRLF;
end;

//function TVersionManifest.GetVersion: TVersion;
//begin
//
//end;
//
procedure TVersionManifest.SetAsString(const AValue: string);
var
  s :TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := AValue;
    App     := s.Values['App'];
    Version := s.Values['Version'];
    Date    := s.Values['Date'];
    Hint    := s.Values['Hint'];
  finally
    s.Free;
  end;
end;

constructor TVersionManifest.Create(const AApp, AVersion, ADate, AHint: string);
begin
  App := AApp;
  Version := AVersion;
  Date := ADate;
  Hint := AHint;
end;

{ TVersion }

function TVersion.GetAsString: string;
var
  i :integer;
begin
  result := IntToStr(Major);
  for i:=1 to High(AsArray) do
    if AsArray[i]=High(UInt32) then
      result := result+'.'
    else
      result := result + '.' + IntToStr(AsArray[i]);
end;

function TVersion.GetAsUInt32: UInt32;
begin
  result := UInt8(Major) shl 24 + UInt8(Minor) shl 16 + UInt8(Patch) shl 8 + UInt8(Build);
end;

procedure TVersion.SetAsString(const AValue: string);
var
  p0 :integer = 0;
  p :integer = 1;
  i :integer = 0;
  item :string;
begin
  while (p<=Length(AValue)) and (i<4) do begin
    if AValue[p]='.' then begin
      item := copy(AValue, p0+1, p-p0-1);
      if item='' then
        AsArray[i] := High(UInt32)
      else
        AsArray[i] := StrToInt(item);
      inc(i);
      p0 := p;
    end;
    inc(p);
  end;
end;

procedure TVersion.SetUint32(AValue: UInt32);
begin
  Major := (AValue and $FF000000) shr 24;
  Minor := (AValue and $00FF0000) shr 16;
  Patch := (AValue and $0000FF00) shr 8;
  Build := (AValue and $000000FF);
end;

//class operator TVersion.>(const Op1, Op2: TVersion): boolean;
//var
//  i :integer;
//begin
//  for i:=0 to High(Op1.AsArray) do begin
////    if AsArray[0]
//    //if
//    //result := Op1
//  end;
//end;
//
constructor TVersion.Create(const str: string);
begin
  self.AsString := str;
end;

end.

