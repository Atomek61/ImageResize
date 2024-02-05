unit PresentationDlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, ComCtrls, Presentations, LCLIntf, LCLType, RichMemo,
  Logging, Types, GetText, FileUtil, ImgRes,
  LoggingRichMemo, Settings, AppSettings;

type

  { TPresentationDialog }

  TPresentationDialog = class(TForm)
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonExecute: TBitBtn;
    ButtonTargetFromDoc: TBitBtn;
    ButtonOk: TBitBtn;
    ButtonWebShow: TBitBtn;
    ButtonCancel: TBitBtn;
    EditImgTagsFilename: TEdit;
    ImagePreview: TImage;
    LabelTargetFolder: TLabel;
    LabelManagers: TLabel;
    LabelLongDescription: TLabel;
    ListBoxManagers: TListBox;
    MemoMessages: TRichMemo;
    OpenImgTagsDialog: TOpenDialog;
    PanelMain: TPanel;
    PanelManagerFrame: TPanel;
    PanelPresentation: TPanel;
    PanelInfo: TPanel;
    PanelControls: TPanel;
    Splitter1: TSplitter;
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonTargetFromDocClick(Sender: TObject);
    procedure ButtonWebShowClick(Sender: TObject);
    procedure EditImgTagsFilenameChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBoxManagersClick(Sender: TObject);
    procedure ListBoxManagersDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    FManagers :TManagers;
    FManagerIndex :integer;
    FManager :TCustomManager;
    FOuterLogger :TLogger;
    procedure SetManagerIndex(Index :Integer);
    procedure OnFrameSelected(Sender :TObject);
    procedure Scan;
    property ManagerIndex :integer read FManagerIndex write SetManagerIndex;
  public
    function Execute(PresentationSettings :TPresentationSettings; ParamsList :TSettingsList) :boolean;
  end;

var
  PresentationDialog: TPresentationDialog;

implementation

uses
    Math, MainDlg;

resourcestring
  SErrMissingImgTags = '.imgtags file not found.';
  SErrShowWebResourceFmt = 'Cant open resource ''%s''.';
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

procedure TPresentationDialog.ButtonExecuteClick(Sender: TObject);
begin
  if (Trim(EditImgTagsFilename.Text)='') or not FileExists(EditImgTagsFilename.Text) then
    raise Exception.Create(SErrMissingImgTags);
  if Assigned(FManager) then begin
    FManager.ImgTagsFilename := EditImgTagsFilename.Text;
    FManager.Execute;
  end;
end;

procedure TPresentationDialog.ButtonOkClick(Sender: TObject);
var
  Manager :TCustomManager;
begin
  for Manager in FManagers do
    Manager.StoreParams;
end;

procedure TPresentationDialog.ButtonTargetFromDocClick(Sender: TObject);
begin
  if MainDialog.EditTargetFolder.Text<>'' then
    EditImgTagsFilename.Text := IncludeTrailingPathDelimiter(MainDialog.EditTargetFolder.Text) + IMAGEINFOSFILETITLE;
end;

procedure TPresentationDialog.ButtonWebShowClick(Sender: TObject);
var
  Manager :TPresentationManager;
  DocURL :string;
begin
  if (ManagerIndex=-1) or not (
      FManagers[ManagerIndex] is TPresentationManager
      and (Trim(EditImgTagsFilename.Text)<>''
  )) then Exit;
  Manager := FManagers[ManagerIndex] as TPresentationManager;
  if Manager.WebResource = '' then Exit;
  DocURL := ExtractFilePath(EditImgTagsFilename.Text)+Manager.WebResource;
  if not OpenDocument(DocURL) then
    Log(SErrShowWebResourceFmt, [DocURL], llWarning);
end;

procedure TPresentationDialog.EditImgTagsFilenameChange(Sender: TObject);
begin
  MainDialog.Dirty := true;
end;

procedure TPresentationDialog.ButtonBrowseTargetFolderClick(Sender: TObject);
begin
  if Trim(EditImgTagsFilename.Text)<>'' then
    OpenImgTagsDialog.InitialDir := ExtractFilePath(EditImgTagsFilename.Text);
  if OpenImgTagsDialog.Execute then
    EditImgTagsFilename.Text := OpenImgTagsDialog.Filename;
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

procedure TPresentationDialog.SetManagerIndex(Index :integer);
var
  Manager :TCustomManager;
begin
  if FManagerIndex=Index then Exit;
  if Index<0 then
    Index := -1;
  ListBoxManagers.ItemIndex := Index;
  if Index<>-1 then begin
    Manager := FManagers[Index];
    LabelLongDescription.Caption := Manager.LongDescription;
    ImagePreview.Picture.Assign(Manager.Preview);
    with Manager.ShowFrame(PanelManagerFrame) do begin
      Align := alClient;
    end;
    ButtonWebShow.Enabled := true;
  end else begin
    LabelLongDescription.Caption := '';
    ImagePreview.Picture := nil;
    ButtonWebShow.Enabled := false;
  end;
  if FManagerIndex<>-1 then begin
    FManager.HideFrame;
  end;
  FManagerIndex := Index;
  if FManagerIndex=-1 then
    FManager := nil
  else
    FManager := FManagers[Index];
end;

function TPresentationDialog.Execute(PresentationSettings :TPresentationSettings; ParamsList :TSettingsList) :boolean;
var
  Manager :TCustomManager;
  Settings :TSettings;
  Index :integer;
begin

  FOuterLogger := TLogger.SwapDefaultLogger(TRichMemoLogger.Create(MemoMessages));
  try
    if FManagers.Count=0 then
      Scan;

    for Settings in ParamsList.Values do begin
      if FManagers.TryFind(Settings.Section, Index) then begin
        FManagers[Index].Settings.Copy(Settings, cmWeak);
        FManagers[Index].Settings.Dirty := false;
      end;
    end;

    EditImgTagsFilename.Text := PresentationSettings.ImgTagsFilename.AsDisplay;
    if FManagers.TryFind(PresentationSettings.Id.Value, Index) then
      ManagerIndex := Index
    else
      ManagerIndex := -1;

    result := ShowModal = mrOk;
    if result then begin
      PresentationSettings.ImgTagsFilename.AsDisplay := EditImgTagsFilename.Text;
      if FManagerIndex<>-1 then
        PresentationSettings.Id.AsText := FManagers[FManagerIndex].Id;

      for Manager in FManagers do begin
        if Manager.Settings.Dirty then begin
          if not ParamsList.TryGetValue(Manager.Settings.Section, Settings) then begin
            Settings := TSettings.Create(Manager.Settings.Section);
            ParamsList.Add(Settings.Section, Settings);
          end;
          Settings.Copy(Manager.Settings, cmDeep);
        end;
      end;
    end;

  finally
    TLogger.SwapDefaultLogger(FOuterLogger).Free;
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

end.

