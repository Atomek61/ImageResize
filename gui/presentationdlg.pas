unit presentationdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Messages, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, webprocessor, LCLIntf, LCLType, logging,
  Types, gettext, FileUtil;

type

  { TPresentationDialog }

  TPresentationDialog = class(TForm)
    Bevel4: TBevel;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonClose: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonExecute: TBitBtn;
    EditFolder: TEdit;
    GroupBoxParams: TGroupBox;
    ImagePreview: TImage;
    ImageList64x44: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    LabelLongDescription: TLabel;
    ListBoxProcessors: TListBox;
    PanelInfo: TPanel;
    PanelControls: TPanel;
    SelectFolderDialog: TSelectDirectoryDialog;
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure EditFolderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxProcessorsClick(Sender: TObject);
    procedure ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FProcessorIndex :integer;
    FProcessors :TProcessors;
    function GetProcessor: TCustomProcessor;
    function GetProcessorId: string;
    procedure OnFrameSelected(Sender :TObject);
    procedure SetProcessorId(AValue: string);
    procedure SetProcessorIndex(AValue: integer);
  public
    procedure Scan;
    property ProcessorId :string read GetProcessorId write SetProcessorId;
    property Processor :TCustomProcessor read GetProcessor;
    property ProcessorIndex :integer read FProcessorIndex write SetProcessorIndex;
  end;

var
  PresentationDialog: TPresentationDialog;

implementation

uses
    Math, MainDlg;

const
  WEBFOLDER = 'web';

resourcestring
  SErrMissingFolder = 'Folder missing or doesnt exists.';

{$R *.lfm}

{ TPresentationDialog }

procedure TPresentationDialog.FormCreate(Sender: TObject);
begin
  FProcessorIndex := -1;
end;

procedure TPresentationDialog.ButtonExecuteClick(Sender: TObject);
begin
  if (Trim(EditFolder.Text)='') or not DirectoryExists(EditFolder.Text) then
    raise Exception.Create(SErrMissingFolder);
  Processor.TargetFolder := EditFolder.Text;
  Processor.Execute;
end;

procedure TPresentationDialog.EditFolderChange(Sender: TObject);
begin
  MainDialog.Dirty := true;
end;

procedure TPresentationDialog.ButtonBrowseTargetFolderClick(Sender: TObject);
begin
  if Trim(EditFolder.Text)<>'' then
    SelectFolderDialog.InitialDir := EditFolder.Text;
  if SelectFolderDialog.Execute then
    EditFolder.Text := SelectFolderDialog.Filename;
end;

procedure TPresentationDialog.FormShow(Sender: TObject);
begin
  Scan;
  if FProcessors.Count>0 then
    ProcessorIndex := 0;
end;

procedure TPresentationDialog.ListBoxProcessorsClick(Sender: TObject);
begin
  ProcessorIndex := ListBoxProcessors.ItemIndex;
end;

procedure TPresentationDialog.ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Cnvs :TCanvas;
  ProcessorIcon :TGraphic;
  hr, hi:integer;
begin
  Cnvs := ListBoxProcessors.Canvas;
  Cnvs.Brush.Color := IfThen(odSelected in State, clHighlight, clBtnFace);
  Cnvs.FillRect(ARect);
  ProcessorIcon := FProcessors[Index].Icon;
  if Assigned(ProcessorIcon) then begin
    hi := ProcessorIcon.Height;
    hr := ARect.Height;
    Cnvs.Draw(ARect.Left+4, ARect.Top+(hr-hi) div 2, ProcessorIcon);
  end;
  Cnvs.Font.Color := IfThen(odSelected in State, clHighlightText, clBtnText);
  Cnvs.Font.Size := 11;
  Cnvs.Font.Style := [fsBold];
  Cnvs.TextOut(78, ARect.Top+10, FProcessors[Index].Title);
  Cnvs.Font.Size := 9;
  Cnvs.Font.Style := [];
  Cnvs.TextOut(78, ARect.Top+36, FProcessors[Index].Description);
end;

function TPresentationDialog.GetProcessorId: string;
begin
  if ListBoxProcessors.ItemIndex = -1 then Exit('');
  result := FProcessors[ListBoxProcessors.ItemIndex].Id;
end;

function TPresentationDialog.GetProcessor: TCustomProcessor;
begin
  if FProcessorIndex = -1 then
    result := nil
  else
    result := FProcessors[FProcessorIndex];
end;

procedure TPresentationDialog.SetProcessorId(AValue: string);
begin
//  ListBoxProcessors.ItemIndex :=
end;

procedure TPresentationDialog.SetProcessorIndex(AValue: integer);
var
  ParamsFrame :TFrame;
begin
  if FProcessorIndex=AValue then Exit;
  if AValue<0 then
    AValue := -1;
  FProcessorIndex := AValue;
  ListBoxProcessors.ItemIndex := AValue;
  if Assigned(Processor) then begin
    LabelLongDescription.Caption := Processor.LongDescription;
    ImagePreview.Picture.Assign(Processor.Preview);
    if GroupBoxParams.ControlCount>0 then
      GroupBoxParams.Controls[0].Parent := nil;
    ParamsFrame := Processor.Frame;
    if Assigned(ParamsFrame) then begin
      ParamsFrame.Parent := GroupBoxParams;
      ParamsFrame.Align := alTop;
      ParamsFrame.Visible := true;
    end;
    ButtonExecute.Enabled := true;
  end else begin
    LabelLongDescription.Caption := '';
    ImagePreview.Picture := nil;
    if GroupBoxParams.ControlCount>0 then
      GroupBoxParams.Controls[0].Hide;
    ButtonExecute.Enabled := false;
  end;
end;

procedure TPresentationDialog.OnFrameSelected(Sender: TObject);
begin
end;

procedure TPresentationDialog.Scan;
var
  Folder :string;
  wp :TCustomProcessor;
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

