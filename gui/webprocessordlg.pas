unit webprocessordlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, webprocessor, LCLIntf, LCLType, logging,
  Types, ImagesMod, gettext;

type

  { TWebProcessorDialog }

  TWebProcessorDialog = class(TForm)
    Bevel4: TBevel;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonClose: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    EditSlideshowFolder: TEdit;
    ImagePreview: TImage;
    ImageList64x44: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelLongDescription: TLabel;
    ListBoxProcessors: TListBox;
    PanelInfo: TPanel;
    PanelParams: TPanel;
    PanelControls: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxProcessorsClick(Sender: TObject);
    procedure ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FProcessorIndex :integer;
    FProcessors :TWebProcessors;

    function GetProcessor: TWebProcessor;
    function GetProcessorId: string;
    procedure OnFrameSelected(Sender :TObject);
    procedure SetProcessorId(AValue: string);
    procedure SetProcessorIndex(AValue: integer);
  public
    procedure Scan;
    property ProcessorId :string read GetProcessorId write SetProcessorId;
    property Processor :TWebProcessor read GetProcessor;
    property ProcessorIndex :integer read FProcessorIndex write SetProcessorIndex;
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

procedure TWebProcessorDialog.FormCreate(Sender: TObject);
begin
  FProcessorIndex := -1;
end;

procedure TWebProcessorDialog.ListBoxProcessorsClick(Sender: TObject);
begin
  ProcessorIndex := ListBoxProcessors.ItemIndex;
end;

procedure TWebProcessorDialog.ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Cnvs :TCanvas;
  ProcessorIcon :TGraphic;
  wi, hi, wr, hr :integer;
begin
  Cnvs := ListBoxProcessors.Canvas;
  Cnvs.Brush.Color := IfThen(odSelected in State, clHighlight, clBtnFace);
  Cnvs.FillRect(ARect);
  ProcessorIcon := FProcessors[Index].Icon;
  wi := ProcessorIcon.Width;
  hi := ProcessorIcon.Height;
  wr := ARect.Width;
  hr := ARect.Height;
  Cnvs.Draw(ARect.Left+4, ARect.Top+(hr-hi) div 2, ProcessorIcon);
  Cnvs.Font.Color := IfThen(odSelected in State, clHighlightText, clBtnText);
  Cnvs.Font.Size := 11;
  Cnvs.Font.Style := [fsBold];
  Cnvs.TextOut(78, ARect.Top+10, FProcessors[Index].Title);
  Cnvs.Font.Size := 9;
  Cnvs.Font.Style := [];
  Cnvs.TextOut(78, ARect.Top+36, FProcessors[Index].Description);
end;

function TWebProcessorDialog.GetProcessorId: string;
begin
  if ListBoxProcessors.ItemIndex = -1 then Exit('');
  result := FProcessors[ListBoxProcessors.ItemIndex].Id;
end;

function TWebProcessorDialog.GetProcessor: TWebProcessor;
begin
  if FProcessorIndex = -1 then
    result := nil
  else
    result := FProcessors[FProcessorIndex];
end;

procedure TWebProcessorDialog.SetProcessorId(AValue: string);
begin
//  ListBoxProcessors.ItemIndex :=
end;

procedure TWebProcessorDialog.SetProcessorIndex(AValue: integer);
begin
  if FProcessorIndex=AValue then Exit;
  if AValue<0 then
    AValue := -1;
  FProcessorIndex := AValue;
  LabelLongDescription.Caption := Processor.LongDescription;
  ListBoxProcessors.ItemIndex := AValue;
  ImagePreview.Picture.Assign(Processor.Preview);
end;

procedure TWebProcessorDialog.OnFrameSelected(Sender: TObject);
begin
end;

procedure TWebProcessorDialog.Scan;
const
  VSPACE = 8;
  HSPACE = 8;
var
  Folder :string;
  wp :TWebProcessor;
  i, y, w, h :integer;
begin
  ProcessorIndex := -1;
  Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.Exename)+WEBFOLDER);
  FreeAndNil(FProcessors);
  FProcessors := TWebProcessor.Scan(Folder);
  ListBoxProcessors.Items.Clear;
  for wp in FProcessors do
    ListBoxProcessors.Items.Add(wp.Title);
end;

end.

