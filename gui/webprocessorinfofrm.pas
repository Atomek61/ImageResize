unit webprocessorinfofrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons, imagesmod,
  Graphics;

type

  TWebProcessorInfoFrame = class(TFrame)
    Preview: TImage;
    LabelDescription: TLabel;
    procedure FrameClick(Sender: TObject);
    procedure FrameMouseEnter(Sender: TObject);
    procedure FrameMouseLeave(Sender: TObject);
  private
    FSelected :boolean;
    FOnSelectedChanged :TNotifyEvent;
    FHover :boolean;
    procedure SetSelected(AValue: boolean);
  public
    property Selected :boolean read FSelected write SetSelected;
    property OnSelectedChanged :TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;
  end;

implementation

uses
  graphtype, Math;

{$R *.lfm}

{ TWebProcessorInfoFrame }

procedure TWebProcessorInfoFrame.FrameMouseEnter(Sender: TObject);
begin
  FHover := true;
  Color := PANELDARKCOLOR;
end;

procedure TWebProcessorInfoFrame.FrameClick(Sender: TObject);
begin
  Selected := true;
end;

procedure TWebProcessorInfoFrame.FrameMouseLeave(Sender: TObject);
begin
  FHover := false;
  if Selected then
    Color := PANELLIGHTCOLOR
  else
    Color := clWindow;
end;

procedure TWebProcessorInfoFrame.SetSelected(AValue: boolean);
begin
  if FSelected=AValue then Exit;
  FSelected:=AValue;
  if Assigned(FOnSelectedChanged) then
    FOnSelectedChanged(self);
  if Selected then
    Color := PANELLIGHTCOLOR
  else
    Color := clWindow;
end;

end.

