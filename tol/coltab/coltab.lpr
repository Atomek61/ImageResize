program coltab;

uses
  Classes, SysUtils, webutils;

function CamelCase(const s :string) :string;
var
  i, n :integer;

  function UpperChar(c :char) :char;
  begin
    result := char(ord(c) - 32) ;
  end;

begin
  n := Length(s);
  result := s;
  for i:=1 to n do
    if (i=1) or (result[i-1]=' ') then
      result[i] := UpperChar(result[i]);
end;

// RemoveSpace
function toName(const s :string) :string;
var
  i :integer;
begin
  result := '';
  for i:=1 to Length(s) do
    if s[i]<>' ' then
      result := result + s[i];
end;

var
  s, d :TStringList;
  i :integer;
  c :TStringArray;
  n, n1 :integer;
  l :string;
  Display, Name :string;
begin
  s := TStringList.Create;
  d := TStringList.Create;
  try
    s.LoadFromFile('../../res/colors/web139.csv');

    // Pass I - find the longest name
    n1 := 0;
    for i:=0 to s.Count-1 do begin
      c := s[i].split(',');
      Name := toName(c[0]);
      if Length(Name)>n1 then n1 := Length(Name);
    end;
    n1 := ((n1+1) div 2 + 1) * 2;

    // Pass II - build lines
    d := TStringList.Create;
    n := s.Count;
    for i:=0 to n-1 do begin
      c := s[i].split(',');
      Display := CamelCase(c[0]);
      Name := toName(Display);
      l := Format('(Name: ''%s'';%*sColor: $%6.6x; Display: ''%s'')', [Name, n1-Length(Name),'', WebColorToColor(c[1]), Display]);
      if i<n-1 then l := l + ',';
      WriteLn(l);
      d.Add(l);
    end;
    d.SaveToFile(Format('../../src/web%d.inc', [n]), TEncoding.UTF8);
  finally
    s.Free;
    d.Free;
  end;
end.

