unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

function StrToIntegerArray(const Str :string; Separator :char; out Values :TIntegerDynArray) :boolean;
function StrToStringArray(const Str :string; Separator :char; out Values :TStringDynArray) :boolean;
function StrToSingleArray(const Str :string; Separator :char; out Values :TSingleDynArray; const FormatSettings :TFormatSettings) :boolean;
function RemoveQuotes(const Str :string) :string;

function IsPathAbsolute(const Path :string) :boolean;

implementation

function IsPathAbsolute(const Path :string) :boolean;
begin
   result := (Length(Path)>0) and IsPathDelimiter(Path, 1) or (Length(Path)>1) and (Path[2]=':');
end;

function StrToIntegerArray(const Str :string; Separator :char; out Values :TIntegerDynArray) :boolean;
var
  i, p, n, x :integer;
begin
  SetLength(Values, 0);
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=Separator) then begin
      if not TryStrToInt(Copy(Str, p, i-p), x) then Exit(false);
      SetLength(Values, n+1);
      Values[n] := x;
      p := i+1;
      inc(n);
    end;
  end;
  result := true;
end;

function StrToStringArray(const Str :string; Separator :char; out Values :TStringDynArray) :boolean;
var
  i, p, n :integer;
  x :string;
begin
  SetLength(Values, 0);
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=Separator) then begin
      x := Copy(Str, p, i-p);
      SetLength(Values, n+1);
      Values[n] := x;
      p := i+1;
      inc(n);
    end;
  end;
  result := true;
end;

function StrToSingleArray(const Str :string; Separator :char; out Values :TSingleDynArray; const FormatSettings :TFormatSettings) :boolean;
var
  i, p, n :integer;
  x :single;
begin
  SetLength(Values, 0);
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=Separator) then begin
      if not TryStrToFloat(Copy(Str, p, i-p), x, FormatSettings) then Exit(false);
      SetLength(Values, n+1);
      Values[n] := x;
      p := i+1;
      inc(n);
    end;
  end;
  result := true;
end;

function RemoveQuotes(const Str :string) :string;
var
  s :string;
begin
  s := Trim(Str);
  if (Length(s)>=2) and ((s[1]='"') and (s[Length(s)]='"')) or ((s[1]='''') and (s[Length(s)]='''')) then
    result := Copy(s, 2, Length(s)-2)
  else
    result := s;
end;

end.

