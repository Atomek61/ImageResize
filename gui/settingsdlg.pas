unit settingsdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, AppSettings, animator;

type

  { TSettingsDialog }

  TBehavior = (bhStandard, bhComfortable, bhNeurotic, bhChucky);

  TSettingsDialog = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel4: TBevel;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    CheckBoxRelPathes: TCheckBox;
    CheckBoxWarnDirty: TCheckBox;
    CheckBoxAutoSave: TCheckBox;
    CheckBoxStopOnError: TCheckBox;
    ComboBoxBehavior: TComboBox;
    ComboBoxThreadsUsed: TComboBox;
    Image1: TImage;
    Image2: TImage;
    ImageListChucky: TImageList;
    LabelProcessing: TLabel;
    LabelBehavior: TLabel;
    Label8: TLabel;
    LabelCores: TLabel;
    LabelSaving: TLabel;
    PaintBoxChucky: TPaintBox;
    PanelControls: TPanel;
    procedure CheckBoxAutoSaveClick(Sender: TObject);
    procedure ComboBoxBehaviorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxChuckyPaint(Sender: TObject);
  private
    FBehaviorLocked :boolean;
    FBehavior :TBehavior;
    FChuckyAnimator :TAnimator;
    procedure SetBehavior(AValue: TBehavior);
    procedure OnAnimateChucky(Sender :TObject; Value :double);
  public
    procedure SetProcessingSettings(Settings: TProcessingSettings);
    procedure GetProcessingSettings(Settings :TProcessingSettings);
    procedure SetDialogSettings(Settings: TDialogSettings);
    procedure GetDialogSettings(Settings :TDialogSettings);
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
  CheckBoxRelPathes.Checked := true;
  FBehaviorLocked := false;
  //with ImageChucky do
  //   PaintBoxChucky.SetBounds(Left, Top, Width, Height);
end;

procedure TSettingsDialog.FormDestroy(Sender: TObject);
begin
  FChuckyAnimator.Free;
end;

procedure TSettingsDialog.FormShow(Sender: TObject);
begin
  with FChuckyAnimator do begin
    Min := 0;
    Max := ImageListChucky.Height;
    OnAnimate := @OnAnimateChucky;
    Duration := CHUCKYSPEED;
    Gradient := agEaseOut2;
  end;
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
    PaintBoxChucky.Visible := true;
  end else begin
    PaintBoxChucky.Visible := false;
    FChuckyAnimator.Stop;
  end;

  FBehaviorLocked := false;
end;

procedure TSettingsDialog.OnAnimateChucky(Sender: TObject; Value :double);
begin
  PaintBoxChucky.Invalidate;
end;

procedure TSettingsDialog.PaintBoxChuckyPaint(Sender: TObject);
begin
  ImageListChucky.Draw(PaintBoxChucky.Canvas, 0, PaintBoxChucky.Height - round(FChuckyAnimator.Value), 0);
//  PaintBoxChucky.Canvas.Draw(0, , ImageChucky.Picture.Graphic);
end;

procedure TSettingsDialog.GetProcessingSettings(Settings :TProcessingSettings);
var
  ThreadsUsed :integer;
begin
  if SameText(ComboBoxThreadsUsed.Text, 'Single') then
    ThreadsUsed := 1
  else if SameText(ComboBoxThreadsUsed.Text, 'Maximum') then
    ThreadsUsed := 0
  else if not TryStrToInt(ComboBoxThreadsUsed.Text, ThreadsUsed) or (ThreadsUsed<0) then
    raise Exception.CreateFmt(SErrInvalidCoresUsedFmt, [ComboBoxThreadsUsed.Text]);
  Settings.ThreadsUsed.Value := ThreadsUsed;
  Settings.StopOnError.Value := CheckBoxStopOnError.Checked;
end;

procedure TSettingsDialog.SetDialogSettings(Settings: TDialogSettings);
begin
  CheckBoxAutoSave.Checked := Settings.AutoSave.Value;
  CheckBoxWarnDirty.Checked := Settings.WarnDirty.Value;
  CheckBoxRelPathes.Checked := Settings.RelPathes.Value;
  CheckBoxAutoSaveClick(nil);
end;

procedure TSettingsDialog.GetDialogSettings(Settings: TDialogSettings);
begin
  Settings.AutoSave.Value := CheckBoxAutoSave.Checked;
  Settings.WarnDirty.Value := CheckBoxWarnDirty.Checked;
  Settings.RelPathes.Value := CheckBoxRelPathes.Checked;
end;

procedure TSettingsDialog.SetProcessingSettings(Settings: TProcessingSettings);
begin
  if Settings.ThreadsUsed.Value = 0 then
    ComboBoxThreadsUsed.Text := 'Maximum'
  else if Settings.ThreadsUsed.Value = 1 then
    ComboBoxThreadsUsed.Text := 'Single'
  else
    ComboBoxThreadsUsed.Text := IntToStr(Settings.ThreadsUsed.Value);
  CheckBoxStopOnError.Checked := Settings.StopOnError.Value;
end;

end.

