unit projectsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Settings;

type

  { TProjectSettings }

  TProjectSettings = class(TSettings)
  public
    ProjectDescription :TStringSetting;
    SourceFolder :TStringSetting;
    SourceMasks :TStringSetting;
    SourceFilenames :TStringSetting;
    TargetFolder :TStringSetting;
    Sizes :TStringSetting;
    JPEGQuality :TStringSetting;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

{ TProjectSettings }

constructor TProjectSettings.Create;
begin
  inherited Create;

end;

destructor TProjectSettings.Destroy;
begin
  inherited Destroy;
end;

end.

