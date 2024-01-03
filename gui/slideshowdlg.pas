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
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  SlideshowDialog: TSlideshowDialog;

implementation

uses
  webprocessor;

{$R *.lfm}

{ TSlideshowDialog }

procedure TSlideshowDialog.FormShow(Sender: TObject);
begin
  TWebProcessor.Create('D:\Mf\Dev\Lazarus\ImageResize\bin\slideshow.wpr');
end;

end.

