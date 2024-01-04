unit webprocessorinfofrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComboEx;

type

  { TWebProcessorInfoFrame }
  TWebProcessorInfoFrameClass = class of TWebProcessorInfoFrame;

  TWebProcessorInfoFrame = class(TFrame)
    ImageIcon: TImage;
    LabelDescription: TLabel;
    LabelTitle: TLabel;
    procedure ImageIconClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TWebProcessorInfoFrame }

procedure TWebProcessorInfoFrame.ImageIconClick(Sender: TObject);
begin

end;

end.

