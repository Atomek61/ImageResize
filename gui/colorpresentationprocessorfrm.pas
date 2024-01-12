unit colorpresentationprocessorfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ColorBox, ExtCtrls,
  PresentationProcessorFrm, colorfrm, System.UITypes;

type

  { TColorPresentationProcessorFrame }

  TColorPresentationProcessorFrame = class(TPresentationProcessorFrame)
    ColorFrameTitle: TColorFrame;
    ColorFrameSymbols: TColorFrame;
    ColorFrameBackground: TColorFrame;
    GroupBox1: TGroupBox;
    Image1: TImage;
    LabelSymbolColor: TLabel;
    LabelTitleColor: TLabel;
    LabelBackgroundColor: TLabel;
    Label5: TLabel;
    Shape1: TShape;
  private
  public
    constructor Create(AOwner :TComponent); override;
  end;


implementation

{$R *.lfm}

{ TColorPresentationProcessorFrame }

constructor TColorPresentationProcessorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColorFrameTitle.Default := TColorRec.DarkSlateGray;
  ColorFrameBackground.Default := TColorRec.FireBrick;
  ColorFrameBackground.Default := TColorRec.PaleTurquoise;
end;

end.

