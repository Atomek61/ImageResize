unit colorpresentationprocessorfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ColorBox, PresentationProcessorFrm;

type

  { TColorPresentationProcessorFrame }

  TColorPresentationProcessorFrame = class(TPresentationProcessorFrame)
    ColorBoxButtonColor: TColorBox;
    ColorBoxTitleColor: TColorBox;
    ColorBoxBackgroundColor: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private
  public
  end;


implementation

{$R *.lfm}

end.

