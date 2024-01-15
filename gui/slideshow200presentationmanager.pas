unit slideshow200presentationmanager;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, presentations{, colorpresentationprocessorfrm};

type
  TSlideshow200PresentationProcessor = class(TColorPresentationManager)
  end;

implementation

initialization
begin
  TManagers.Register(TSlideshow200PresentationProcessor);
end;


end.

