unit webprocessordlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, webprocessor, LCLIntf, LCLType, logging;

type

  { TWebProcessorDialog }

  TWebProcessorDialog = class(TForm)
    Bevel4: TBevel;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    EditSlideshowFolder: TEdit;
    Image1: TImage;
    ImageList32: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    PanelControls: TPanel;
    ScrollBox: TScrollBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FWebProcessors :TWebProcessors;
  public
    procedure Scan;
  end;

var
  WebProcessorDialog: TWebProcessorDialog;

implementation

uses
  webprocessorinfofrm;

const
  WEBFOLDER = 'web';

{$R *.lfm}

{ TWebProcessorDialog }

procedure TWebProcessorDialog.FormShow(Sender: TObject);
begin
//  TWebProcessor.Create('D:\Mf\Dev\Lazarus\ImageResize\bin\slideshow.wpr');
  Scan;
end;

procedure TWebProcessorDialog.FormCreate(Sender: TObject);
begin

end;

procedure TWebProcessorDialog.Scan;
const
  LSPACE = 120;
  VSPACE = 8;
  HSPACE = 8;
var
  Folder :string;
  wp :TWebProcessor;
  i, y, w, h :integer;
  Frame :TWebProcessorInfoFrame;
  RadioButton :TRadioButton;
begin
//  try
    Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.Exename)+WEBFOLDER);
    FreeAndNil(FWebProcessors);
    FWebProcessors := TWebProcessor.Scan(Folder);
    with ScrollBox do while ControlCount>0 do
      Controls[ControlCount-1].Free;
    i := 1;
    y := VSPACE;
    w := ScrollBox.ClientWidth-LSPACE-HSPACE-GetSystemMetrics(SM_CXVSCROLL);
    for wp in FWebProcessors do begin
      RadioButton := TRadioButton.Create(self);
      RadioButton.Name := Format('RadioButtonWebProcessor%d', [i]);
      RadioButton.Caption := Format('&%d %s', [i, wp.Caption]);
      RadioButton.SetBounds(HSPACE, y+VSPACE, LSPACE-HSPACE, RadioButton.Height);
      RadioButton.Parent := ScrollBox;
      RadioButton.TabStop := true;
      RadioButton.Visible := true;

      Frame := wp.GetInfoFrameClass.Create(self);
      Frame.Name := Format('InfoFrame%d', [i]);
      Frame.LabelTitle.Caption := wp.Title;
      Frame.LabelDescription.Caption := wp.Description;
      if wp.IconFile<>'' then
        Frame.ImageIcon.Picture.LoadFromFile(wp.Folder+wp.IconFile);
      h := Frame.Height;
      Frame.SetBounds(LSPACE, y, w, h);
      Frame.Parent := ScrollBox;
      Frame.Visible := true;
      inc(i);
      inc(y, h+VSPACE);
    end;
  //except on E: Exception.
  //end;
end;

end.

