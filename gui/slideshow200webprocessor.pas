unit slideshow200webprocessor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, webprocessor;

type
  TSlideshow200WebProcessor = class(TWebProcessor)
  end;

implementation

initialization
begin
  TWebProcessor.Register(TSlideshow200WebProcessor);
end;


end.

