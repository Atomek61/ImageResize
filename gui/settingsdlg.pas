unit settingsdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Settings;

type

  { TSettingsDialog }

  TSettingsDialog = class(TForm)
    ButtonOk: TButton;
    ButtonCancel: TButton;
    CheckBoxStopOnError: TCheckBox;
    ComboBoxThreadsUsed: TComboBox;
    Image1: TImage;
    Label19: TLabel;
    Label8: TLabel;
    LabelCores: TLabel;
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure SetProcessingSettings(Value: TProcessingSettings);
    procedure GetProcessingSettings(Value :TProcessingSettings);
  end;

var
  SettingsDialog: TSettingsDialog;

resourcestring
    SCptCoresFmt = 'for %d Cores';
    SErrInvalidCoresUsedFmt = 'Invalid number of threads ''%s''.';


implementation

{$R *.lfm}

{ TSettingsDialog }

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin
  LabelCores.Caption := Format(SCptCoresFmt, [TThread.ProcessorCount]);
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

