unit webprocessordlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, webprocessor, LCLIntf, LCLType, logging,
  webprocessorinfofrm, Types, ImagesMod, gettext;

type

  { TWebProcessorDialog }

  TWebProcessorDialog = class(TForm)
    Bevel4: TBevel;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonClose: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    EditSlideshowFolder: TEdit;
    ImageList64x44: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBoxProcessors: TListBox;
    PanelParams: TPanel;
    PanelControls: TPanel;
    procedure FormShow(Sender: TObject);
    procedure ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FProcessors :TWebProcessors;
    FInfoFrames :array of TWebProcessorInfoFrame;
    FSelectedFrame :TWebProcessorInfoFrame;
    function GetProcessorId: string;
    function GetSelectedProcessor: TWebProcessor;
    procedure OnRadioButtonClick(Sender :TObject);
    procedure OnFrameSelected(Sender :TObject);
    procedure SetProcessorId(AValue: string);
  public
    procedure Scan;
    property ProcessorId :string read GetProcessorId write SetProcessorId;
    property SelectedProcessor :TWebProcessor read GetSelectedProcessor;
  end;

var
  WebProcessorDialog: TWebProcessorDialog;

implementation

uses
    Math;

const
  WEBFOLDER = 'web';

{$R *.lfm}

{ TWebProcessorDialog }

procedure TWebProcessorDialog.FormShow(Sender: TObject);
begin
  Scan;
end;

procedure TWebProcessorDialog.ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Cnvs :TCanvas;
begin
  Cnvs := ListBoxProcessors.Canvas;
  Cnvs.Brush.Color := IfThen(odSelected in State, clHighlight, clBtnFace);
  Cnvs.FillRect(ARect);
  SelectedProcessor.Icon.Draw(Cnvs, ARect);
//  ImageList64x44.Draw(Cnvs, ARect.Left+4, ARect.Top+(ARect.Height-ImageList64x44.Height) div 2, 0);
  Cnvs.Font.Color := IfThen(odSelected in State, clHighlightText, clBtnText);
  Cnvs.Font.Size := 11;
  Cnvs.Font.Style := [fsBold];
  Cnvs.TextOut(78, ARect.Top+10, FProcessors[Index].Title);
  Cnvs.Font.Size := 9;
  Cnvs.Font.Style := [];
  Cnvs.TextOut(78, ARect.Top+36, FProcessors[Index].Description);
end;

procedure TWebProcessorDialog.OnRadioButtonClick(Sender: TObject);
begin
end;

function TWebProcessorDialog.GetProcessorId: string;
begin
  if ListBoxProcessors.ItemIndex = -1 then Exit('');
  result := FProcessors[ListBoxProcessors.ItemIndex].Id;
end;

function TWebProcessorDialog.GetSelectedProcessor: TWebProcessor;
begin
  result := FProcessors[ListBoxProcessors.ItemIndex];
end;

procedure TWebProcessorDialog.SetProcessorId(AValue: string);
begin
//  ListBoxProcessors.ItemIndex :=
end;

procedure TWebProcessorDialog.OnFrameSelected(Sender: TObject);
begin
  if Sender=FSelectedFrame then Exit;
  if Assigned(FSelectedFrame) then
    FSelectedFrame.Selected := false;
  if Assigned(Sender) then
    (Sender as TWebProcessorInfoFrame).Selected := true;
  FSelectedFrame := TWebProcessorInfoFrame(Sender);
end;

procedure TWebProcessorDialog.Scan;
const
  VSPACE = 8;
  HSPACE = 8;
var
  Folder :string;
  wp :TWebProcessor;
  i, y, w, h :integer;
  Frame :TWebProcessorInfoFrame;
//  RadioButton :TRadioButton;
begin
  Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.Exename)+WEBFOLDER);
  FreeAndNil(FProcessors);
  FProcessors := TWebProcessor.Scan(Folder);
  ListBoxProcessors.Items.Clear;
  for wp in FProcessors do
    ListBoxProcessors.Items.Add(wp.Title);
  //i := 0;
  //y := VSPACE;
  //w := ScrollBox.ClientWidth-2*HSPACE-GetSystemMetrics(SM_CXVSCROLL);
//
//
//    Frame := wp.GetInfoFrameClass.Create(self);
//    Frame.Name := Format('InfoFrame%d', [i]);
//    Frame.LabelTitle.Caption := wp.Title;
//    Frame.LabelDescription.Caption := wp.Description;
//    if wp.IconFile<>'' then
//      Frame.ImageIcon.Picture.LoadFromFile(wp.Folder+wp.IconFile);
//    h := Frame.Height;
//    Frame.SetBounds(HSPACE, y, w, h);
//    Frame.Parent := ScrollBox;
//    Frame.OnSelectedChanged := @OnFrameSelected;
//    Frame.Visible := true;
    //SetLength(FInfoFrames, i+1);
    //FInfoFrames[i] := Frame;
    //inc(i);
    //inc(y, h+VSPACE);
end;

end.

