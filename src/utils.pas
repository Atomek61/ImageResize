unit utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Types, Generics.Collections;

function StrToIntegerArray(const Str :string; Separator :char; out Values :TIntegerDynArray) :boolean;
function StrToStringArray(const Str :string; Separator :char = ',') :TStringArray;
function StrToSingleArray(const Str :string; Separator :char; out Values :TSingleDynArray; const FormatSettings :TFormatSettings) :boolean;
function RemoveQuotes(const Str :string) :string;
function TryStrToPlaceholders(const Str :string; Del :char; out Placeholders :TStringArray) :boolean;
function TryParsePlaceholderParams(const Str :string; Del: char; out Params :TStringArray) :boolean;

function IsPathAbsolute(const Path :string) :boolean;
function IsWildcard(const Str :string) :boolean;

implementation

type
  TStringArrayHelper = specialize TArrayHelper<string>;

// Absolute path begins with a backslash or the second char is :
function IsPathAbsolute(const Path :string) :boolean;
begin
   result := (Length(Path)>0) and IsPathDelimiter(Path, 1) or (Length(Path)>1) and (Path[2]=':');
end;

function StrToIntegerArray(const Str :string; Separator :char; out Values :TIntegerDynArray) :boolean;
var
  i, p, n, x :integer;
  item :string;
begin
  SetLength(Values, 0);
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=Separator) then begin
      item := Trim(Copy(Str, p, i-p));
      if item<>'' then begin
        if not TryStrToInt(item, x) then Exit(false);
        SetLength(Values, n+1);
        Values[n] := x;
        inc(n);
      end;
      p := i+1;
    end;
  end;
  result := true;
end;

function StrToStringArray(const Str :string; Separator :char) :TStringArray;
var
  i, p, n :integer;
  x :string;
begin
  result := nil;
  p := 1;
  n := 0;
  for i:=1 to Length(Str)+1 do begin
    if (i>Length(Str)) or (Str[i]=Separator) then begin
      x := Trim(Copy(Str, p, i-p));
      SetLength(result, n+1);
      result[n] := x;
      p := i+1;
      inc(n);
    end;
  end;
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

function TryStrToPlaceholders(const Str :string; Del :char; out Placeholders :TStringArray) :boolean;
var
  p0, p1, n :integer;
begin
  n := 0;
  p1 := 0;
  SetLength(Placeholders, 0);
  while true do begin
    p0 := PosEx(Del, Str, p1+1);
    if p0=0 then break;
    p1 := PosEx(Del, Str, p0+1);
    if p1=0 then Exit(false);
    SetLength(Placeholders, n+1);
    Placeholders[n] := Copy(Str, p0+1, p1-p0-1);
    inc(n);
  end;
  TStringArrayHelper.sort(Placeholders);
  result := true;
end;

function TryParsePlaceholderParams(const Str :string; Del: char; out Params :TStringArray) :boolean;
var
  p :integer;
begin
  Params := nil;
  p := Pos(Del, Str);
  Params := StrToStringArray(Copy(Str, p+1, Length(Str)-p), ',');
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

function IsWildcard(const Str :string) :boolean;
begin
  result := (Pos('*', Str)>0) or (Pos('?', Str)>0);
end;

end.

