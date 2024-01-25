unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniFiles, settings;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSettings :TSettings;
    procedure OnTitleSettingChanged(Sender :TObject);
    procedure OnSizeSettingChanged(Sender :TObject);
    procedure OnTumbSettingChanged(Sender :TObject);
    procedure OnSexSettingChanged(Sender :TObject);
    procedure OnWeatherSettingChanged(Sender :TObject);
    procedure OnSettingsChanged(Sender :TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini :TIniFile;
  i :integer;
  Sex :TPicklistSetting;
  Weather :TPicktextSetting;
begin
  FSettings := TSettings.Create('Project');
  Ini := TIniFile.Create('settings.definition.ini');
  try
    FSettings.LoadDef(Ini);
  finally
    Ini.Free;
  end;

  Label1.Caption := FSettings['Title'].Caption;
  Edit1.Text := FSettings['Title'].AsText;
  FSettings['Title'].OnChanged := @OnTitleSettingChanged;

  Label2.Caption := FSettings['Size'].Caption;
  Edit2.Text := FSettings['Size'].AsText;
  FSettings['Size'].OnChanged := @OnSizeSettingChanged;

  CheckBox1.Caption := FSettings['Tumb'].Caption;
  CheckBox1.Checked := (FSettings['Tumb'] as TBooleanSetting).Value;
  FSettings['Tumb'].OnChanged := @OnTumbSettingChanged;

  Sex := FSettings['Sex'] as TPicklistSetting;
  Label3.Caption := Sex.Caption;
  for i:=0 to High(Sex.Display) do
    ComboBox1.Items.Add(Sex.Display[i]);
  ComboBox1.ItemIndex := Sex.ItemIndex;
  FSettings['Sex'].OnChanged := @OnSexSettingChanged;

  Weather := FSettings['Weather'] as TPicktextSetting;
  Label4.Caption := Weather.Caption;
  for i:=0 to High(Weather.Display) do
    ComboBox2.Items.Add(Weather.Display[i]);
  ComboBox2.Text := Weather.AsDisplay;
  FSettings['Weather'].OnChanged := @OnWeatherSettingChanged;

  FSettings.OnChanged := @OnSettingsChanged;

end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  FSettings['Title'].AsText := Edit1.Text;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  (FSettings['Tumb'] as TBooleanSetting).Value := CheckBox1.Checked;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create('settings.ini');
  try
    FSettings.Save(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create('settings.ini');
  try
    FSettings.Load(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FSettings.SetDefaults;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  (FSettings['Sex'] as TPicklistSetting).ItemIndex := ComboBox1.ItemIndex;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  (FSettings['Weather'] as TPicktextSetting).AsText := ComboBox2.Text;
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  FSettings['Size'].AsText := Edit2.Text;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FSettings.Free;
end;

procedure TForm1.OnTitleSettingChanged(Sender: TObject);
begin
  Edit1.Text := (Sender as TSetting).AsText;
end;

procedure TForm1.OnSizeSettingChanged(Sender: TObject);
begin
  Edit2.Text := (Sender as TSetting).AsText;
end;

procedure TForm1.OnTumbSettingChanged(Sender: TObject);
begin
  CheckBox1.Checked := (Sender as TBooleanSetting).Value;
end;

procedure TForm1.OnSexSettingChanged(Sender: TObject);
begin
  ComboBox1.ItemIndex := (Sender as TPicklistSetting).ItemIndex;
end;

procedure TForm1.OnWeatherSettingChanged(Sender: TObject);
begin
  ComboBox2.Text := (Sender as TPicktextSetting).AsDisplay;
end;

procedure TForm1.OnSettingsChanged(Sender: TObject);
begin
  if FSettings.Dirty then
    Panel1.Color := clRed
  else
    Panel1.Color := clDefault;

end;

end.

