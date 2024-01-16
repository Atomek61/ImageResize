unit PresentationDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, ComCtrls, presentations, LCLIntf, LCLType, Arrow, ValEdit, RichMemo,
  logging, Types, gettext, FileUtil, ListFilterEdit, ShortPathEdit,
  LoggingRichMemo, Settings, IniFiles;

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
    ListBoxManagers: TListBox;
    MemoMessages: TRichMemo;
    PanelPresentation: TPanel;
    PanelInfo: TPanel;
    PanelControls: TPanel;
    SelectFolderDialog: TSelectDirectoryDialog;
    ValueListEditor1: TValueListEditor;
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure EditTargetFolderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxManagersClick(Sender: TObject);
    procedure ListBoxManagersDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FManagerIndex :integer;
    FManagers :TManagers;
    FOuterLogger :TLogger;
    FPresentationSettingsList :TSettingsList;
    FPresentationSettings :TPresentationSettings;
    function GetManager: TCustomManager;
    function GetManagerId: string;
    procedure OnFrameSelected(Sender :TObject);
    procedure SetManagerId(AValue: string);
    procedure SetManagerIndex(AValue: integer);
  public
    function Execute(PresentationSettings :TPresentationSettings; PresentationSettingsList :TSettingsList) :boolean;
    procedure Scan;
    property ManagerId :string read GetManagerId write SetManagerId;
    property Manager :TCustomManager read GetManager;
    property ManagerIndex :integer read FManagerIndex write SetManagerIndex;
  end;

  { TPresentationSettings }

  TPresentationSettings = class(TSettings)
  private
    FManagerId :string;
    FTargetFolder :string;
    FTargetTitle :string;
    procedure SetManagerId(AValue: string);
    procedure SetTargetFolder(AValue: string);
    procedure SetTargetTitle(AValue: string);
  public
    constructor Create; override;
    procedure Defaults; override;
    procedure SaveToIni(Ini :TCustomIniFile); override;
    procedure LoadFromIni(Ini :TCustomIniFile); override;
    property ManagerId :string read FManagerId write SetManagerId;
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
  FManagerIndex := -1;
  FManagers := TManagers.Create;
end;

procedure TPresentationDialog.FormDestroy(Sender: TObject);
begin
  FManagers.Free;
end;

procedure TPresentationDialog.FormShow(Sender: TObject);
begin
  Scan;
  ManagerId := FPresentationSettings.ManagerId;
  EditTargetFolder.Text := FPresentationSettings.TargetFolder;
  EditTargetTitle.Text := FPresentationSettings.TargetTitle;
  //if FManagers.Count>0 then
  //  ManagerIndex := 0;
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
  Manager.TargetFolder := EditTargetFolder.Text;
  Manager.TargetTitle := EditTargetTitle.Text;
  Manager.Execute;
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

procedure TPresentationDialog.ListBoxManagersClick(Sender: TObject);
begin
  ManagerIndex := ListBoxManagers.ItemIndex;
end;

procedure TPresentationDialog.ListBoxManagersDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Cnvs :TCanvas;
  ManagerIcon :TGraphic;
  hr, hi:integer;
begin
  Cnvs := ListBoxManagers.Canvas;
  Cnvs.Brush.Color := IfThen(odSelected in State, clHighlight, clBtnFace);
  Cnvs.FillRect(ARect);
  ManagerIcon := FManagers[Index].Icon;
  if Assigned(ManagerIcon) then begin
    hi := ManagerIcon.Height;
    hr := ARect.Height;
    Cnvs.Draw(ARect.Left+4, ARect.Top+(hr-hi) div 2, ManagerIcon);
  end;
  Cnvs.Font.Color := IfThen(odSelected in State, clHighlightText, clBtnText);
  Cnvs.Font.Size := 11;
  Cnvs.Font.Style := [fsBold];
  Cnvs.TextOut(78, ARect.Top+10, FManagers[Index].Title);
  Cnvs.Font.Size := 9;
  Cnvs.Font.Style := [];
  Cnvs.TextOut(78, ARect.Top+36, FManagers[Index].Description);
end;

function TPresentationDialog.GetManagerId: string;
begin
  if ListBoxManagers.ItemIndex = -1 then Exit('');
  result := FManagers[ListBoxManagers.ItemIndex].Id;
end;

procedure TPresentationDialog.SetManagerId(AValue: string);
begin
  if ManagerId=AValue then Exit;
  ManagerIndex := FManagers.IndexOf(AValue);
end;

function TPresentationDialog.GetManager: TCustomManager;
begin
  if FManagerIndex = -1 then
    result := nil
  else
    result := FManagers[FManagerIndex];
end;

procedure TPresentationDialog.SetManagerIndex(AValue: integer);
var
  ParamsFrame :TFrame;
  Settings :TSettings;
begin
  if FManagerIndex=AValue then Exit;
  if AValue<0 then
    AValue := -1;
  FManagerIndex := AValue;
  ListBoxManagers.ItemIndex := AValue;
  if Assigned(Manager) then begin
    LabelLongDescription.Caption := Manager.LongDescription;
    ImagePreview.Picture.Assign(Manager.Preview);
    if GroupBoxParams.ControlCount>0 then
      GroupBoxParams.Controls[0].Parent := nil;
//    Log('Manager.Id='+Manager.Id);
    if FPresentationSettingsList.TryGetValue(Manager.Id, Settings) then
      Manager.Settings := Settings;
    ParamsFrame := Manager.Frame;
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

function TPresentationDialog.Execute(PresentationSettings :TPresentationSettings; PresentationSettingsList :TSettingsList) :boolean;
begin
  FPresentationSettings := PresentationSettings;
  FPresentationSettingsList := PresentationSettingsList;
  result :=  ShowModal = mrOk;
  if result then begin
    FPresentationSettings.ManagerId   := ManagerId;
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
  wp :TCustomManager;
begin
  ManagerIndex := -1;
  Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.Exename)+PRESENTATIONS_FOLDER);
  FManagers.Clear;
  TPresentationManager.Scan(Folder, FManagers);
  ListBoxManagers.Items.Clear;
  for wp in FManagers do
    ListBoxManagers.Items.Add(wp.Title);
end;

{ TPresentationSettings }

procedure TPresentationSettings.SetManagerId(AValue: string);
begin
  if FManagerId=AValue then Exit;
  FManagerId := AValue;
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
  FManagerId := '';
end;

procedure TPresentationSettings.SaveToIni(Ini: TCustomIniFile);
begin
  inherited SaveToIni(Ini);
  Ini.WriteString(Section, 'ManagerId', FManagerId);
  Ini.WriteString(Section, 'TargetFolder', FTargetFolder);
  Ini.WriteString(Section, 'TargetTitle', FTargetTitle);
end;

procedure TPresentationSettings.LoadFromIni(Ini: TCustomIniFile);
begin
  inherited LoadFromIni(Ini);
  FManagerId := Ini.ReadString(Section, 'ManagerId', '');
  FTargetFolder := Ini.ReadString(Section, 'TargetFolder', '');
  FTargetTitle := Ini.ReadString(Section, 'TargetTitle', '');
end;

initialization
begin
  TSettings.Register(TPresentationSettings);
end;

end.

