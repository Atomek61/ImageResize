unit slideshow200presentationprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, presentationprocessor, colorpresentationprocessorfrm;

type
  TSlideshow200PresentationProcessor = class(TColorPresentationProcessor)
  end;

implementation

initialization
begin
  TProcessors.Register(TSlideshow200PresentationProcessor);
end;


end.

