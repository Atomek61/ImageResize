unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Settings,
  IniFiles;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ComboBox: TComboBox;
    ComboBox1: TComboBox;
    EditTitle: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FSettings :TSettings;
    procedure OnSpeedChanged(Sender :TObject);
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
  s :string;
begin
  FSettings := TSettings.Create('Settings');
  Ini := TIniFile.Create('settings.definition.ini');
  FSettings.Load(Ini, 'Settings');



  with FSettings['Speed'] as TPicklistSetting do begin
    OnChanged := @self.OnSpeedChanged;
    if Mode=TPicklistSetting.TMode.pmList then
      ComboBox.Style := csDropDownList;
    for s in DisplayItems do
      ComboBox.Items.Add(s);
    ComboBox.ItemIndex := ItemIndex;
  end;

end;

procedure TForm1.OnSpeedChanged(Sender: TObject);
begin
  ComboBox.Text := FSettings['Speed'].Display;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FSettings['Title'].Display := EditTitle.Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FSettings['Speed'].Display := ComboBox.Text;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  (FSettings['Speed'] as TPicklistSetting).ItemIndex := -1;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FSettings['Speed'].Display := 'Hallo';
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ComboBox1.Text := 'Hallo';
end;

procedure TForm1.ComboBoxChange(Sender: TObject);
begin
  FSettings['Speed'].Display := ComboBox.Text;
end;

end.

