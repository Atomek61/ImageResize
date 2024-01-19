unit webcolors;

{$mode delphi}

interface

uses
  Classes, SysUtils, webutils, Generics.Collections;

type
  TColorDictionary = TDictionary<string, TColorInfo>;

const
  WEBCOLORINFOS :array[0..138] of TColorInfo = (
{$i web139.inc}
);

  function WebColorsDict :TColorDictionary;

implementation

var
  FWebColorsDict :TColorDictionary = nil;

function WebColorsDict: TColorDictionary;
var
  i :integer;
begin
  if not Assigned(FWebColorsDict) then begin
    FWebColorsDict := TColorDictionary.Create(Length(WEBCOLORINFOS));
    for i:=0 to High(WEBCOLORINFOS) do
      FWebColorsDict.Add(LowerCase(WEBCOLORINFOS[i].Name), WEBCOLORINFOS[i]);
  end;
  result := FWebColorsDict;
end;

finalization
begin
  FWebColorsDict.Free;
end;

end.

