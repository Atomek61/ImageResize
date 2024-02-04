unit StringUtils;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function LoadStringFromFile(const Filename :string) :string;

implementation

function LoadStringFromFile(const Filename :string) :string;
var
  s :TStringList;
begin
  s := TStringList.Create;
  try
    s.LoadFromFile(Filename);
    result := s.Text;
  finally
    s.Free;
  end;

end;

end.

