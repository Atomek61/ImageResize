unit settingsdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, Settings, animator;

type

  { TSettingsDialog }

  TBehavior = (bhStandard, bhComfortable, bhNeurotic, bhChucky);

  TSettingsDialog = class(TForm)
    Bevel1: TBevel;
    Bevel4: TBevel;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    CheckBoxWarnDirty: TCheckBox;
    CheckBoxAutoSave: TCheckBox;
    CheckBoxStopOnError: TCheckBox;
    ComboBoxBehavior: TComboBox;
    ComboBoxThreadsUsed: TComboBox;
    Image1: TImage;
    Image2: TImage;
    ImageChucky: TImage;
    LabelBehavior: TLabel;
    Label8: TLabel;
    LabelCores: TLabel;
    PanelControls: TPanel;
    TimerChucky: TTimer;
    procedure CheckBoxAutoSaveClick(Sender: TObject);
    procedure ComboBoxBehaviorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImageChuckyClick(Sender: TObject);
    procedure TimerChuckyTimer(Sender: TObject);
  private
    FBehaviorLocked :boolean;
    FBehavior :TBehavior;
    FChuckyT0 :TDateTime;
    FChuckyAnimator :TAnimator;
    procedure SetBehavior(AValue: TBehavior);
    procedure OnAnimateChucky(Sender :TObject; Value :double);
  public
    procedure SetProcessingSettings(Value: TProcessingSettings);
    procedure GetProcessingSettings(Value :TProcessingSettings);
    procedure SetDialogSettings(Value: TDialogSettings);
    procedure GetDialogSettings(Value :TDialogSettings);
    property Behavior :TBehavior read FBehavior write SetBehavior;
  end;

var
  SettingsDialog: TSettingsDialog;

resourcestring
  SCptCoresFmt = 'for %d Cores';
  SErrInvalidCoresUsedFmt = 'Invalid number of threads ''%s''';
                                    //  AutoSave   WarnDirty
  SCptBehavior0 = 'Standard';      //      0          1
  SCptBehavior1 = 'Comfortable';   //      1          0
  SCptBehavior2 = 'Neurotic';      //      1          1
  SCptBehavior3 = 'Chuck Norris';  //      0          0

implementation

uses
  maindlg;

{$R *.lfm}

const
  BEHAVIORMATRIX :array[boolean] of array[boolean] of TBehavior = ((bhChucky, bhStandard), (bhComfortable, bhNeurotic));
  CHUCKYSPEED = 1.5; // secs to appear

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin
  FChuckyAnimator := TAnimator.Create;
  FChuckyAnimator.Resolution := 20;
  LabelCores.Caption := Format(SCptCoresFmt, [TThread.ProcessorCount]);
  ComboBoxBehavior.Items.Add(SCptBehavior0);
  ComboBoxBehavior.Items.Add(SCptBehavior1);
  ComboBoxBehavior.Items.Add(SCptBehavior2);
  ComboBoxBehavior.Items.Add(SCptBehavior3);
  FBehaviorLocked := true;
  FBehavior := bhStandard;
  ComboBoxBehavior.ItemIndex := integer(bhStandard);
  CheckBoxAutoSave.Checked := false;
  CheckBoxWarnDirty.Checked := true;
  FBehaviorLocked := false;
  ImageChucky.Top := ClientHeight;
end;

procedure TSettingsDialog.FormDestroy(Sender: TObject);
begin
  FChuckyAnimator.Free;
end;

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  with FChuckyAnimator do begin
    Min := 0;
    Max := ImageChucky.Height;
    OnAnimate := @OnAnimateChucky;
    Duration := CHUCKYSPEED;
    Gradient := agEaseOut2;
  end;
end;

procedure TSettingsDialog.ImageChuckyClick(Sender: TObject);
begin

end;

procedure TSettingsDialog.CheckBoxAutoSaveClick(Sender: TObject);
begin
  if not FBehaviorLocked then
    Behavior := BEHAVIORMATRIX[CheckBoxAutoSave.Checked, CheckBoxWarnDirty.Checked];
end;

procedure TSettingsDialog.ComboBoxBehaviorChange(Sender: TObject);
begin
  if not FBehaviorLocked then
    Behavior := TBehavior(ComboBoxBehavior.ItemIndex);
end;

procedure TSettingsDialog.TimerChuckyTimer(Sender: TObject);
var
  T1 :TDateTime;
  ChuckyVisibleLines :integer;
begin
  T1 := Now;
  ChuckyVisibleLines := round(ImageChucky.Height*(T1-FChuckyT0)*SECSPERDAY/CHUCKYSPEED);
  if ChuckyVisibleLines >= ImageChucky.Height then begin
    TimerChucky.Enabled := false;
    ChuckyVisibleLines := ImageChucky.Height;
  end;
  ImageChucky.Top := ClientHeight - ChuckyVisibleLines;
end;

procedure TSettingsDialog.SetBehavior(AValue: TBehavior);
begin
  if FBehavior=AValue then Exit;
  FBehavior:=AValue;
  FBehaviorLocked := true;
  ComboBoxBehavior.ItemIndex := integer(FBehavior);
  CheckBoxAutoSave.Checked := FBehavior in [bhNeurotic, bhComfortable];
  CheckBoxWarnDirty.Checked := FBehavior in [bhNeurotic, bhStandard];
  if Behavior = bhChucky then begin
    FChuckyAnimator.Start;
    ImageChucky.Visible := true;
  end else begin
    ImageChucky.Top := ClientHeight;
    ImageChucky.Visible := false;
    FChuckyAnimator.Stop;
  end;

  FBehaviorLocked := false;
end;

procedure TSettingsDialog.OnAnimateChucky(Sender: TObject; Value :double);
var
  ChuckyVisibleLines :integer;
begin
  ChuckyVisibleLines := round(Value);
  ImageChucky.Top := ClientHeight - ChuckyVisibleLines;
end;

procedure TSettingsDialog.GetProcessingSettings(Value :TProcessingSettings);
var
  ThreadsUsed :integer;
begin
  if SameText(ComboBoxThreadsUsed.Text, 'Single') then
    ThreadsUsed := 1
  else if SameText(ComboBoxThreadsUsed.Text, 'Maximum') then
    ThreadsUsed := 0
  else if not TryStrToInt(ComboBoxThreadsUsed.Text, ThreadsUsed) or (ThreadsUsed<0) then
    raise Exception.CreateFmt(SErrInvalidCoresUsedFmt, [ComboBoxThreadsUsed.Text]);
  Value.ThreadsUsed := ThreadsUsed;
  Value.StopOnError := CheckBoxStopOnError.Checked;
end;

procedure TSettingsDialog.SetDialogSettings(Value: TDialogSettings);
begin
  CheckBoxAutoSave.Checked := Value.AutoSave;
  CheckBoxWarnDirty.Checked := Value.WarnDirty;
  CheckBoxAutoSaveClick(nil);
end;

procedure TSettingsDialog.GetDialogSettings(Value: TDialogSettings);
begin
  Value.AutoSave := CheckBoxAutoSave.Checked;
  Value.WarnDirty := CheckBoxWarnDirty.Checked;
end;

procedure TSettingsDialog.SetProcessingSettings(Value: TProcessingSettings);
begin
  if Value.ThreadsUsed = 0 then
    ComboBoxThreadsUsed.Text := 'Maximum'
  else if Value.ThreadsUsed = 1 then
    ComboBoxThreadsUsed.Text := 'Single'
  else
    ComboBoxThreadsUsed.Text := IntToStr(Value.ThreadsUsed);
  CheckBoxStopOnError.Checked := Value.StopOnError;
end;

end.

