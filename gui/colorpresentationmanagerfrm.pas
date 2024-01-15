unit colorpresentationmanagerfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ColorBox, ExtCtrls,
  presentationmanagerfrm, colorfrm, Graphics, System.UITypes, ImagesMod;

type

  { TColorPresentationmanagerFrame }

  TColorPresentationmanagerFrame = class(TPresentationManagerFrame)
    ColorFrameTitle: TColorFrame;
    ColorFrameSymbols: TColorFrame;
    ColorFrameBackground: TColorFrame;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label5: TLabel;
    Shape1: TShape;
  private
  public
    constructor Create(AOwner :TComponent); override;
  end;


implementation

{$R *.lfm}

{ TColorPresentationmanagerFrame }

constructor TColorPresentationmanagerFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColorFrameTitle.Default := clBlack;
  ColorFrameSymbols.Default := clRed;
  ColorFrameBackground.Default := clWhite;
end;

end.

