unit datetimeutils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  SErrDateFmt = 'Cant ''%s'' convert to a date.';



function TryStrToDate(const Str :string; out Date :TDate) :boolean;
function StrToDate(const Str :string) :TDate;
function DateToStr(Date :TDate) :string;

implementation

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

end.

