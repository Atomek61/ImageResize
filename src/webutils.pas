unit webutils;

{$mode delphi}

interface

uses
  Classes, SysUtils, Graphics, FPImage;

type
  TColorInfo = record
    Name :string;
    Color :TColor;
    Display :string;
  end;

function ColorToHTMLColor(Value :TColor) :string;
function TryHTMLColorToColor(const Value :string; out Color :TColor) :boolean;
function HTMLColorToColor(const Value :string) :TColor;

implementation

resourcestring
  SErrInvalidHTMLColorFmt = 'Invalid web color ''%s''.';

function ColorToHTMLColor(Value :TColor) :string;
var
  Color :TFPColor;
begin
  Color := TColorToFPColor(ColorToRGB(Value));
  if Color.Alpha = High(word) then
    result := Format('#%2.2x%2.2x%2.2x', [byte(Color.Red), byte(Color.Green), byte(Color.Blue)])
  else
    result := Format('#%2.2x%2.2x%2.2x%2.2x', [byte(Color.Alpha), byte(Color.Red), byte(Color.Green), byte(Color.Blue)]);
end;

function TryHTMLColorToColor(const Value :string; out Color :TColor) :boolean;
var
  c :TFPColor;
  n :integer;

  function Hex1(i :integer) :word;
  begin
    result := ord(Value[i])-48;
    if result>9 then
      result := (ord(Value[i]) and not $20) - 55;
    result := result shl 8 + result;
  end;

  function Hex2(i :integer) :word;
  begin
    result := Hex1(i) shl 4 + Hex1(i+1);
  end;

  function IsHex :boolean;
  var
    i :integer;
    c :char;
  begin
    for i:=2 to n do begin
      c := Value[i];
      if not ((c>='0') and (c<='9') or (c>='A') and (c<='F') or (c>='a') and (c<='f')) then Exit(False);
    end;
    result := true;
  end;

begin
  n := Length(Value);
  result := (n>3) and (Value[1]='#') and IsHex;
  if result then begin
    case n of
    4:
      begin
        c.Alpha := $FFFF;
        c.red   := Hex1(2);
        c.green := Hex1(3);
        c.blue  := Hex1(4);
      end;
    5:
      begin
        c.Alpha := Hex1(2);
        c.red   := Hex1(3);
        c.green := Hex1(4);
        c.blue  := Hex1(5);
      end;
    7:
      begin
        c.Alpha := $FFFF;
        c.red   := Hex2(2);
        c.green := Hex2(4);
        c.blue  := Hex2(6);
      end;
    9:
      begin
        c.Alpha := Hex2(2);
        c.red   := Hex2(4);
        c.green := Hex2(6);
        c.blue  := Hex2(2);
      end;
    else
      Exit(False);
    end;
    Color := FPColorToTColor(c);
  end;
end;

function HTMLColorToColor(const Value: string): TColor;
begin
  if not TryHTMLColorToColor(Value, result) then
    raise Exception.CreateFmt(SErrInvalidHTMLColorFmt, [Value]);
end;

end.

