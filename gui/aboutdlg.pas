unit aboutdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpClient, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ComCtrls, ActnList, LCLType;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    ActionLicense: TAction;
    ActionVersions: TAction;
    ActionUpdate: TAction;
    ActionCheckUpdate: TAction;
    ActionDownload: TAction;
    ActionList: TActionList;
    ButtonCheckUpdate: TBitBtn;
    ButtonDownload: TBitBtn;
    ButtonClose: TButton;
    FPHTTPClient: TFPHTTPClient;
    Image1: TImage;
    ImageMainIcon: TImage;
    LabelAppVersion: TLabel;
    LabelImgresGuiCpr1: TLabel;
    LabelImgresGuiCpr2: TLabel;
    LabelLatest: TLabel;
    LabelLinkImageResizeHome: TLabel;
    LabelLinkImageResizeDownload: TLabel;
    LabelAvailability: TLabel;
    LabelAvailability1: TLabel;
    LabelImgresGuiCpr: TLabel;
    LabelLinkGithub: TLabel;
    LabelProcessorVersion: TLabel;
    LabelDependencies: TLabel;
    MemoLicense: TMemo;
    PageControl: TPageControl;
    TabSheetVersion: TTabSheet;
    TabSheetUpdate: TTabSheet;
    TabSheetLicense: TTabSheet;
    TimerBlinkAvailable: TTimer;
    ToolBar: TToolBar;
    ToolButtonLicense: TToolButton;
    ToolButtonVersions: TToolButton;
    ToolButtonUpdate: TToolButton;
    procedure ActionCheckUpdateExecute(Sender: TObject);
    procedure ActionDownloadExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LabelLinkGithubClick(Sender: TObject);
    procedure LabelLinkImageResizeDownloadClick(Sender: TObject);
    procedure LabelLinkImageResizeHomeClick(Sender: TObject);
    procedure TabButtonClick(Sender :TObject);
    procedure TimerBlinkAvailableTimer(Sender: TObject);
  private

  public
    class function Execute(const Text1, Text2, LicenseResName :string) :boolean;
  end;

implementation

uses
  maindlg, imgres, LazVersion, BGRABitmapTypes, opensslsockets, Updateutils,
  lclintf, dGlobal, FileInfo;

resourcestring
  SCptAvailable           = 'Available';
  SErrAppNotMatchingFmt   = 'Application ''%s'' not matching, ''%s'' expected';
  SMsgChecking            = 'Checking for update...';
  SMsgUpdateAvailableFmt  = 'Update available: Version %s from %s';
  SMsgIsUpToDate          = 'There is no newer version available';
  SErrUpdateCheckFmt      = 'Update check failed - %s';

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.LabelLinkImageResizeDownloadClick(Sender: TObject);
begin
  ActionDownload.Execute;
end;

procedure TAboutDialog.LabelLinkImageResizeHomeClick(Sender: TObject);
begin
  OpenURL(APPWEBURL);
end;

procedure TAboutDialog.TabButtonClick(Sender: TObject);
begin
  PageControl.TabIndex := TAction(Sender).Tag;
end;

procedure TAboutDialog.TimerBlinkAvailableTimer(Sender: TObject);
begin
  if LabelAvailability.Font.Color = clWindowText then
    LabelAvailability.Font.Color := clRed
  else
    LabelAvailability.Font.Color := clWindowText;
end;

procedure TAboutDialog.ActionCheckUpdateExecute(Sender: TObject);
var
  UpdateManifest :TVersionManifest;
begin
  try
    LabelAvailability.Font.Color := clDefault;
    LabelAvailability.Caption := SMsgChecking;
    Application.ProcessMessages;
    UpdateManifest.AsString := FPHTTPClient.SimpleGet(GUIVERURL);
    with UpdateManifest do
      LabelLatest.Caption := SCptAvailable+#10+#10+App+#10+Version+#10+Date+#10+Hint;
    if UpdateManifest.App<>GUIVER.App then
      raise Exception.CreateFmt(SErrAppNotMatchingFmt, [UpdateManifest.App, GUIVER.App]);
    if GUIVER.AsDateTime < UpdateManifest.AsDateTime then begin
      TimerBlinkAvailable.Enabled := true;
      LabelAvailability.Font.Color := clWindowText;
      LabelAvailability.Caption := Format(SMsgUpdateAvailableFmt, [UpdateManifest.version, UpdateManifest.Date]);
    end else begin
      LabelAvailability.Font.Color := clWindowText;
      LabelAvailability.Caption := SMsgIsUpToDate;
    end;
  except
    on E :Exception do begin
      LabelAvailability.Font.Color := clRed;
      LabelAvailability.Caption := Format(SErrUpdateCheckFmt, [E.Message]);
    end;
  end;

end;

procedure TAboutDialog.ActionDownloadExecute(Sender: TObject);
begin
  OpenURL(APPDOWNLOADURL);
end;

procedure TAboutDialog.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := TabSheetLicense;
end;

procedure TAboutDialog.LabelLinkGithubClick(Sender: TObject);
begin
  OpenURL(APPGITHUBURL);
end;

class function TAboutDialog.Execute(const Text1, Text2, LicenseResName :string): boolean;
var
  AboutDialog: TAboutDialog;
  s :TStream;
  ver :TProgramVersion;
begin
  AboutDialog := TAboutDialog.Create(nil);
  with AboutDialog do try
    LabelImgresGuiCpr.Caption := Text1;
    GetProgramVersion(ver);
    LabelAppVersion.Caption := GUIVER_APP + ' ' + ProgramversionToStr(ver);
    LabelProcessorVersion.Caption := Text2;
    LabelDependencies.Caption := Format(SCptDependenciesFmt, [laz_version, BGRABitmapVersionStr, dGlobal.dExifVersion]);
    s := TResourceStream.Create(HInstance, LicenseResName, RT_RCDATA);
    try
      MemoLicense.Lines.LoadFromStream(s);
    finally
      s.Free;
    end;
    ImageMainIcon.Picture.Icon := Application.Icon;
    result := ShowModal = mrOk;
  finally
    AboutDialog.Free;
  end;
end;

end.

