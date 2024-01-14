unit colorfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, Buttons, Dialogs, FPImage,
  System.UITypes, Graphics, StdCtrls, ImagesMod;

type

  { TColorFrame }

  TColorFrame = class(TFrame)
    ButtonSelect: TBitBtn;
    ButtonDefaultColor: TBitBtn;
    ColorDialog: TColorDialog;
    Label1: TLabel;
    PanelColor: TPanel;
    procedure ButtonDefaultColorClick(Sender: TObject);
    procedure ButtonSelectClick(Sender: TObject);
    procedure PanelColorClick(Sender: TObject);
  private
    FDefault :TColor;
    function GetSelected: TColor;
    procedure SetSelected(AValue: TColor);
  public
    constructor Create(AOwner :TComponent); override;
    property Selected :TColor read GetSelected write SetSelected;
    property Default :TColor read FDefault write FDefault;
  end;

implementation

{$R *.lfm}

{ TColorFrame }

procedure TColorFrame.ButtonSelectClick(Sender: TObject);
begin
  ColorDialog.Color := Selected;
  if ColorDialog.Execute then
    Selected := ColorDialog.Color;
end;

procedure TColorFrame.PanelColorClick(Sender: TObject);
begin
  ButtonSelectClick(nil);
end;

procedure TColorFrame.ButtonDefaultColorClick(Sender: TObject);
begin
  Selected := Default;
end;

function TColorFrame.GetSelected: TColor;
begin
  result := PanelColor.Color;
end;

procedure TColorFrame.SetSelected(AValue: TColor);
begin
  if AValue=Selected then Exit;
  PanelColor.Color := AValue;
end;

constructor TColorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefault := clBlack;
end;

end.

