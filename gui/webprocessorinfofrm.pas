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
  private

  public

  end;

implementation

{$R *.lfm}

end.

