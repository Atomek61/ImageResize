unit updateutils;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils;

type

  { TVersion }

  TVersion = record
    Major :integer;
    Minor :integer;
    Release :integer;
    Build :integer;
    constructor Create(const str :string);
  end;

implementation

{ TVersion }

constructor TVersion.Create(const str: string);
begin

end;

end.

