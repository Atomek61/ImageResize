unit templatetestdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit10R: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit10: TEdit;
    Edit1R: TEdit;
    Edit2R: TEdit;
    Edit3R: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  Templates;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  e :TEngine;
begin
  e := TEngine.Create;
  e.add('INDEX', Edit1.Text);
  e.add('FILENAME', Edit2.Text);
  e.add('RUDI', Edit3.Text);
  e.Solve;
  Edit1R.Text := e.Values[0];
  Edit2R.Text := e.Values[1];
  Edit3R.Text := e.Values[2];
  Edit10R.Text := e.Compile(Edit10.Text);

end;

end.

