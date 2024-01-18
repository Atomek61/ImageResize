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
  end;

implementation

{ TProjectSettings }

end.

