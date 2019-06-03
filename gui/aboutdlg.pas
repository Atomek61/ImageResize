unit aboutdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    Button1: TButton;
    Image1: TImage;
    LabelImgresGuiCpr: TLabel;
    LabelImgresStr: TLabel;
    LabelUrl1: TLabel;
    LabelUrl2: TLabel;
    MemoLicense: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private

  public
    class function Execute(const Text1, Text2, License :string) :boolean;
  end;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutDialog.FormCreate(Sender: TObject);
begin

end;

procedure TAboutDialog.Image1Click(Sender: TObject);
begin

end;

class function TAboutDialog.Execute(const Text1, Text2, License: string): boolean;
var
  AboutDialog: TAboutDialog;
begin
  AboutDialog := TAboutDialog.Create(nil);
  with AboutDialog do try
    LabelImgresGuiCpr.Caption := Text1;
    LabelImgresStr.Caption := Text2;
    MemoLicense.Text := License;
    result := ShowModal = mrOk;
  finally
    AboutDialog.Free;
  end;
end;

end.

