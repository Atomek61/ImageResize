unit language;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, GetText;

type

  { TLanguage }

  TLanguage = class
  private
    class var FCode :string; // 'EN' or 'DE'
  public
    class function Code :string;
    class procedure SetCode(const Value :string);
  end;


implementation

{ TLanguage }

class function TLanguage.Code: string;
begin
  result := FCode;
end;

class procedure TLanguage.SetCode(const Value: string);
begin
  FCode := Copy(LowerCase(Value), 1, 2);
end;

procedure InitLanguage;
var
  Lang :string;
begin
  Lang := '';
  GetLanguageIDs(Lang, TLanguage.FCode)
end;

initialization
begin
  InitLanguage;
end;

end.

