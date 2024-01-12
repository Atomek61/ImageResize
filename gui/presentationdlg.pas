unit PresentationDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls, ComCtrls, PresentationProcessor, LCLIntf, LCLType, RichMemo,
  logging, Types, gettext, FileUtil, LoggingRichMemo, Settings, IniFiles;

type

  TPresentationSettings = class;

  { TPresentationDialog }

  TPresentationDialog = class(TForm)
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonOk: TBitBtn;
    ButtonExecute: TBitBtn;
    ButtonCancel: TBitBtn;
    EditTargetFolder: TEdit;
    EditTargetTitle: TEdit;
    GroupBoxParams: TGroupBox;
    ImagePreview: TImage;
    LabelTargetFolder: TLabel;
    Label2: TLabel;
    LabelTargetTitle: TLabel;
    LabelLongDescription: TLabel;
    ListBoxProcessors: TListBox;
    MemoMessages: TRichMemo;
    PanelPresentation: TPanel;
    PanelInfo: TPanel;
    PanelControls: TPanel;
    SelectFolderDialog: TSelectDirectoryDialog;
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure EditTargetFolderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxProcessorsClick(Sender: TObject);
    procedure ListBoxProcessorsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FProcessorIndex :integer;
    FProcessors :TProcessors;
    FOuterLogger :TLogger;
    FPresentationSettingsList :TSettingsList;
    FPresentationSettings :TPresentationSettings;
    function GetProcessor: TCustomProcessor;
    function GetProcessorId: string;
    procedure OnFrameSelected(Sender :TObject);
    procedure SetProcessorId(AValue: string);
    procedure SetProcessorIndex(AValue: integer);
  public
    procedure Show(PresentationSettings :TPresentationSettings; PresentationSettingsList :TSettingsList);
    procedure Scan;
    property ProcessorId :string read GetProcessorId write SetProcessorId;
    property Processor :TCustomProcessor read GetProcessor;
    property ProcessorIndex :integer read FProcessorIndex write SetProcessorIndex;
  end;

  { TPresentationSettings }

  TPresentationSettings = class(TSettings)
  private
    FProcessorId :string;
    FTargetFolder :string;
    FTargetTitle :string;
    procedure SetProcessorId(AValue: string);
    procedure SetTargetFolder(AValue: string);
    procedure SetTargetTitle(AValue: string);
  public
    constructor Create; override;
    procedure Defaults; override;
    procedure SaveToIni(Ini :TCustomIniFile); override;
    procedure LoadFromIni(Ini :TCustomIniFile); override;
    property ProcessorId :string read FProcessorId write SetProcessorId;
    property TargetFolder :string read FTargetFolder write SetTargetFolder;
    property TargetTitle :string read FTargetTitle write SetTargetTitle;
  end;

var
  PresentationDialog: TPresentationDialog;

implementation

uses
    Math, MainDlg;

resourcestring
  SErrMissingFolder = 'Folder missing or does not exist.';

{$R *.lfm}

{ TPresentationDialog }

procedure TPresentationDialog.FormCreate(Sender: TObject);
begin
  FProcessorIndex := -1;
  FProcessors := TProcessors.Create;
end;

procedure TPresentationDialog.FormDestroy(Sender: TObject);
begin
  FProcessors.Free;
end;

procedure TPresentationDialog.FormShow(Sender: TObject);
begin
  Scan;
  ProcessorId := FPresentationSettings.ProcessorId;
  EditTargetFolder.Text := FPresentationSettings.TargetFolder;
  EditTargetTitle.Text := FPresentationSettings.TargetTitle;
  //if FProcessors.Count>0 then
  //  ProcessorIndex := 0;
  FOuterLogger := TLogger.SwapDefaultLogger(TRichMemoLogger.Create(MemoMessages));
end;

procedure TPresentationDialog.FormHide(Sender: TObject);
begin
  TLogger.SwapDefaultLogger(FOuterLogger).Free;
end;

procedure TPresentationDialog.ButtonExecuteClick(Sender: TObject);
begin
  if (Trim(EditTargetFolder.Text)='') or not DirectoryExists(EditTargetFolder.Text) then
    raise Exception.Create(SErrMissingFolder);
  Processor.TargetFolder := EditTargetFolder.Text;
  Processor.TargetTitle := EditTargetTitle.Text;
  Processor.Execute;
end;

procedure TPresentationDialog.EditTargetFolderChange(Sender: TObject);
begin
  MainDialog.Dirty := true;
end;

procedure TPresentationDialog.ButtonBrowseTargetFolderClick(Sender: TObject);
begin
  if Trim(EditTargetFolder.Text)<>'' then
    SelectFolderDialog.InitialDir := EditTargetFolder.Text;
  if SelectFolderDialog.Execute then
    EditTargetFolder.Text := SelectFolderDialog.Filename;
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

procedure TPresentationDialog.SetProcessorId(AValue: string);
begin
  if ProcessorId=AValue then Exit;
  ProcessorIndex := FProcessors.IndexOf(AValue);
end;

function TPresentationDialog.GetProcessor: TCustomProcessor;
begin
  if FProcessorIndex = -1 then
    result := nil
  else
    result := FProcessors[FProcessorIndex];
end;

procedure TPresentationDialog.SetProcessorIndex(AValue: integer);
var
  ParamsFrame :TFrame;
  Settings :TSettings;
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
//    Log('Processor.Id='+Processor.Id);
    if FPresentationSettingsList.TryGetValue(Processor.Id, Settings) then
      Processor.Settings := Settings;
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

procedure TPresentationDialog.Show(PresentationSettings :TPresentationSettings; PresentationSettingsList :TSettingsList);
begin
  FPresentationSettings := PresentationSettings;
  FPresentationSettingsList := PresentationSettingsList;
  if ShowModal<>mrCancel then begin
    FPresentationSettings.ProcessorId   := ProcessorId;
    FPresentationSettings.TargetFolder  := EditTargetFolder.Text;
    FPresentationSettings.TargetTitle   := EditTargetTitle.Text;
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
  Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.Exename)+PRESENTATIONS_FOLDER);
  FProcessors.Clear;
  TPresentationProcessor.Scan(Folder, FProcessors);
  ListBoxProcessors.Items.Clear;
  for wp in FProcessors do
    ListBoxProcessors.Items.Add(wp.Title);
end;

{ TPresentationSettings }

procedure TPresentationSettings.SetProcessorId(AValue: string);
begin
  if FProcessorId=AValue then Exit;
  FProcessorId := AValue;
  Changed;
end;

procedure TPresentationSettings.SetTargetFolder(AValue: string);
begin
  if FTargetFolder=AValue then Exit;
  FTargetFolder := AValue;
  Changed;
end;

procedure TPresentationSettings.SetTargetTitle(AValue: string);
begin
  if FTargetTitle=AValue then Exit;
  FTargetTitle:=AValue;
  Changed;
end;

constructor TPresentationSettings.Create;
begin
  inherited Create;
  Section := PRESENTATIONSECTION;
end;

procedure TPresentationSettings.Defaults;
begin
  inherited Defaults;
  FTargetFolder := '';
  FProcessorId := '';
end;

procedure TPresentationSettings.SaveToIni(Ini: TCustomIniFile);
begin
  inherited SaveToIni(Ini);
  Ini.WriteString(Section, 'ProcessorId', FProcessorId);
  Ini.WriteString(Section, 'TargetFolder', FTargetFolder);
  Ini.WriteString(Section, 'TargetTitle', FTargetTitle);
end;

procedure TPresentationSettings.LoadFromIni(Ini: TCustomIniFile);
begin
  inherited LoadFromIni(Ini);
  FProcessorId := Ini.ReadString(Section, 'ProcessorId', '');
  FTargetFolder := Ini.ReadString(Section, 'TargetFolder', '');
  FTargetTitle := Ini.ReadString(Section, 'TargetTitle', '');
end;

initialization
begin
  TSettings.Register(TPresentationSettings);
end;

end.

