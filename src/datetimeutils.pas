unit datetimeutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

const
  ISODATETIMEFMT :string = 'YYYY-MM-DD HH:NN:SS.SSS';

resourcestring
  SErrDateFmt = 'Cant ''%s'' convert to a date.';
  SErrInvalidDateTimeFormatFmt = 'Invalid format for datetime ''%s''.';
  SErrInvalidDateTimeFmt = 'Invalid datetime ''%s''.';

function TryStrToDateTime(const str :string; out Value :TDateTime) :boolean;
function StrToDateTime(const str :string) :TDateTime;
function DateTimeToStr(const Value :TDateTime; const Fmt :string = '') :string;

function TryStrToDate(const Str :string; out Date :TDate) :boolean;
function StrToDate(const Str :string) :TDate;
function DateToStr(Date :TDate) :string;

implementation

// Accepts one of
//    12345678901234567890123
//  n YYYY-MM-DD HH:NN:SS.ZZZ
// 19 YYYY-MM-DD HH:NN:SS
// 16 YYYY-MM-DD HH:NN
// 10 YYYY-MM-DD
function TryStrToDateTime(const str :string; out Value :TDateTime) :boolean;
var
  l :integer;
  y, m, d, h, n, s, z :integer;
begin
  l := Length(str);
  if not (((l=10) or (l=16) or (l=19) or ((l>19) and (l<=23)))
    and (str[5]='-')
    and (str[8]='-')
    and TryStrToInt(Copy(str, 1, 4), y)
    and TryStrToInt(Copy(str, 6, 2), m)
    and TryStrToInt(Copy(str, 9, 2), d)) then Exit(false);
  h := 0;
  n := 0;
  s := 0;
  z := 0;
  if l>=16 then begin
    if not ((str[11]=' ')
      and (str[14]=':')
      and TryStrToInt(Copy(str, 12, 2), h)
      and TryStrToInt(Copy(str, 15, 2), n)) then Exit(false);
  end;
  if l>=19 then begin
    if not ((str[17]=':')
      and TryStrToInt(Copy(str, 18, 2), s)) then Exit(false);
  end;
  if l>=20 then begin
    if not ((str[20]='.')
      and TryStrToInt(Copy(str, 21, n-20), z)) then Exit(false);
  end;
  result := TryEncodeDateTime(y, m, d, h, n, s, z, Value);
end;

function StrToDateTime(const str :string) :TDateTime;
begin
  if not TryStrToDateTime(Str, result) then
    raise Exception.CreateFmt(SErrInvalidDateTimeFmt, [str]);
end;

// Fmt = YYYY-MM-HH[ HH:NN[:SS[.|.S|.SS|.SSS]]] | <empty>
// <emtpy> means "auto" this is a best format, depending on the time resulution
function DateTimeToStr(const Value: TDateTime; const Fmt: string): string;
var
  l, prec :integer;
  y, m, d, h, n, s, z :word;
begin
  l := Length(Fmt);
  if l=0 then begin
    // auto
    DecodeDateTime(Value, y, m, d, h, n, s, z);
    if h+n+s+z=0 then
      Exit(DateTimeToStr(Value, Copy(ISODATETIMEFMT, 1, 10)))
    else if s+z=0 then
      Exit(DateTimeToStr(Value, Copy(ISODATETIMEFMT, 1, 16)))
    else if z=0 then
      Exit(DateTimeToStr(Value, Copy(ISODATETIMEFMT, 1, 19)))
    else
      Exit(DateTimeToStr(Value, ISODATETIMEFMT));
  end;
  if ((l<10) or (l>Length(ISODATETIMEFMT)) or (Copy(ISODATETIMEFMT, 1, l)<>Fmt)) then
    raise Exception.CreateFmt(SErrInvalidDateTimeFormatFmt, [Fmt]);
  DecodeDateTime(Value, y, m, d, h, n, s, z);
  if l=10 then
    Exit(Format('%4.4d-%2.2d-%2.2d', [y, m, d]));
  if l=16 then
    Exit(Format('%4.4d-%2.2d-%2.2d %2.2d:%2.2d', [y, m, d, h, n]));
  if l=19 then
    Exit(Format('%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d', [y, m, d, h, n, s]));
  if l>19 then begin
    prec := l-20;
    Exit(Format('%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d.', [y, m, d, h, n, s])+Copy(Format('%*.*d', [prec, prec, z]), 1, prec));
  end;
  raise Exception.CreateFmt(SErrInvalidDateTimeFormatFmt, [Fmt]);
end;


function TryStrToDate(const Str: string; out Date: TDate): boolean;
var
  year, month, day :Longint;
begin
  result := (Length(Str)=10) and (Str[5]='-') and (Str[8]='-')
    and TryStrToInt(Copy(Str, 1, 4), year)
    and TryStrToInt(Copy(Str, 6, 2), month)
    and TryStrToInt(Copy(Str, 9, 2), day);
  if result then
    Date := EncodeDate(year, month, day);
end;

function StrToDate(const Str: string): TDate;
begin
  if not TryStrToDate(Str, result) then
    raise EConvertError.CreateFmt(SErrDateFmt, [Str]);
end;

function DateToStr(Date: TDate): string;
var
  year, month, day :word;
begin
  DecodeDate(Date, year, month, day);
  result := Format('%4.4d-%2.2d-%2.2d', [year, month, day]);
end;

//procedure test;
//var
//  dt :TDateTime;
//begin
//  TryStrToDateTime('2023-12-16', dt);
//  Writeln(DateTimeToStr(dt, 'YYYY-MM-DD'));
//  Writeln(DateTimeToStr(dt));
//  TryStrToDateTime('2023-12-16 08:23', dt);
//  Writeln(DateTimeToStr(dt, 'YYYY-MM-DD HH:NN'));
//  Writeln(DateTimeToStr(dt));
//  TryStrToDateTime('2023-12-16 08:23:56', dt);
//  Writeln(DateTimeToStr(dt, 'YYYY-MM-DD HH:NN:SS'));
//  Writeln(DateTimeToStr(dt));
//  TryStrToDateTime('2023-12-16 08:23:56.123', dt);
//  DateTimeToStr(dt, 'YYYY-MM-DD HH:NN:SS.SSS');
//  DateTimeToStr(dt, 'YYYY-MM-DD HH:NN:SS.SS');
//  DateTimeToStr(dt, 'YYYY-MM-DD HH:NN:SS.S');
//  DateTimeToStr(dt, 'YYYY-MM-DD HH:NN:SS.');
//  Writeln(DateTimeToStr(dt));
//  TryStrToDateTime('2023-12-16 08', dt);
//end;
//
//initialization
//begin
//  test;
//end;
//
end.

