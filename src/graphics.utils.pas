unit graphics.utils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

function FitRect(const r1, r2 :TRect) :TRect;

implementation

function FitRect(const r1, r2 :TRect) :TRect;
var
  f1, f2 :single;
  w, h :integer;
begin
  if r1.IsEmpty then
    result := r2
  else if r2.IsEmpty then
    result := r1
  else begin
    f1 := r1.Width/r1.Height;
    f2 := r2.Width/r2.Height;
    if f1>=f2 then begin
      result.Left := r2.Left;
      result.Right := r2.Right;
      h := round(result.Width/f1);
      result.Top := r2.Top + (r2.Height - h) div 2;
      result.Bottom := result.Top + h;
    end else begin
      result.Top := r2.Top;
      result.Bottom := r2.Bottom;
      w := round(result.Height*f1);
      result.Left := r2.Left + (r2.Width - w) div 2;
      result.Right := result.Left + w;
    end;
  end;
end;

end.

