unit slideshowdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  StdCtrls, ExtCtrls;

type

  { TSlideshowDialog }

  TSlideshowDialog = class(TForm)
    Bevel4: TBevel;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    EditSlideshowFolder: TEdit;
    Image1: TImage;
    ImageList32: TImageList;
    Label1: TLabel;
    PanelControls: TPanel;
  private

  public

  end;

var
  SlideshowDialog: TSlideshowDialog;

implementation

{$R *.lfm}

end.

