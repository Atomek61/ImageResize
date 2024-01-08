unit slideshow200webprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, WebProcessor, ColorWebProcessorFrm;

type
  TSlideshow200WebProcessor = class(TColorWebProcessor)
  end;

implementation

initialization
begin
  TProcessors.Register(TSlideshow200WebProcessor);
end;


end.

