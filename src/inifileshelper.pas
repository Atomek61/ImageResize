unit inifileshelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TCustomIniFileHelper }

  TCustomIniFileHelper = class helper for TCustomIniFile
    function StringRead(const Section, Key :string) :string;
    function ReadLang(const Section, Key, Lang :string) :string; overload;
    function ReadLang(const Section, Key, Default, Lang :string) :string; overload;
  end;

implementation

resourcestring
  SErrSectionKeyNotFoundFmt = 'Key ''%s'' not found in section ''%s''.';


const
  MAGICVALUE = '%x?#';

{ TCustomIniFileHelper }

function TCustomIniFileHelper.StringRead(const Section, Key: string): string;
begin
  result := ReadString(Section, Key, MAGICVALUE);
  if result = MAGICVALUE then
    raise Exception.CreateFmt(SErrSectionKeyNotFoundFmt, [Key, Section]);
end;

function TCustomIniFileHelper.ReadLang(const Section, Key, Lang: string): string;
begin
  result := ReadString(Section, Key+'.'+Lang, MAGICVALUE);
  if result = MAGICVALUE then
    result := StringRead(Section, Key);
end;

function TCustomIniFileHelper.ReadLang(const Section, Key, Default, Lang: string): string;
begin
  result := ReadString(Section, Key+'.'+Lang, MAGICVALUE);
  if result = MAGICVALUE then begin
    result := ReadString(Section, Key, MAGICVALUE);
    if result = MAGICVALUE then
      result := Default;
  end;
end;

end.

