unit colorwebprocessorfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ColorBox, WebProcessorFrm;

type

  { TColorWebProcessorFrame }

  TColorWebProcessorFrame = class(TWebProcessorFrame)
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

