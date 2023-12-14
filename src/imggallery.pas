unit imggallery;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

{
%GAL.TITLE%
%GAL.DESCRIPTION%

%INDEX%
%NUMBER%

%ORG.FILENAME%
%ORG.FILEPATH%
%ORG.FILETITLE%
%ORG.FILEEXT%
%ORG.FILEDATE%
%ORG.FILESIZE%
%ORG.WIDTH%
%ORG.HEIGHT%
%ORG.SIZE%
%ORG.RATIO%
%ORG.EXIF.DESCRIPTION%
%ORG.EXIF.DATE%
%ORG.EXIF.COPYRIGHT%
%ORG.TAG.TITLE%
%ORG.TAG.DESCRIPTION%
%ORG.TAG.DATE%
%ORG.TAG.COPYRIGHT%
%ORG.DESCRIPTION%

%DST.SIZE%
%DST.FILENAME%
%DST.FILEPATH%
%DST.FILETITLE%
%DST.FILEEXT%
%DST.FILEDATE%
%DST.FILESIZE%
}

interface

uses
  Classes, SysUtils;

type

  { TImgGalleryProcessor }

  TImgGalleryProcessor = class
  public type

    { TParams }

    TParams = record
      SrcFilenames :array of string;
      DstFolder :string;
      Sizes :array of integer;
      procedure Defaults;
    end;

  private
    FParams :TParams;
  public
    constructor Create;
    destructor Destroy; override;
    class function GetVersion: string;
    function Execute :boolean;
  end;

implementation

{ TImgGalleryProcessor.TParams }

procedure TImgGalleryProcessor.TParams.Defaults;
begin
  SetLength(SrcFilenames, 0);
  DstFolder := '';
  SetLength(Sizes, 0);
end;

{ TImgGalleryProcessor }

constructor TImgGalleryProcessor.Create;
begin

end;

destructor TImgGalleryProcessor.Destroy;
begin
  inherited Destroy;
end;

class function TImgGalleryProcessor.GetVersion: string;
begin

end;

function TImgGalleryProcessor.Execute: boolean;
begin
  // I. Sammle Infos über Originaldateien

  // II. Iteriere über alle Originaldateien

  // III. Iteriere über die Quellcodedateien und ersetze die Platzhalter

  // IV. Speichere die erzeugten Zieldateien
end;

end.

