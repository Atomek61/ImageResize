unit aboutdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    FPHTTPClient: TFPHTTPClient;
    ImageMainIcon: TImage;
    LabelImgresGuiCpr: TLabel;
    LabelImgresStr: TLabel;
    LabelDependencies: TLabel;
    LabelUrl2: TLabel;
    MemoLicense: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private

  public
    class function Execute(const Text1, Text2, License :string) :boolean;
  end;

implementation

uses
  maindlg, imgres, LazVersion, BGRABitmapTypes, opensslsockets;

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAboutDialog.BitBtn1Click(Sender: TObject);
var
  str :AnsiString;
begin
  str := FPHTTPClient.Get(CHECKUPDATEURL);
  MemoLicense.Text := str;
end;

class function TAboutDialog.Execute(const Text1, Text2, License: string): boolean;
var
  AboutDialog: TAboutDialog;
//  FileVerInfo: TFileVersionInfo;
begin
{  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    writeln('File version: ',FileVerInfo.VersionStrings.Values['FileVersion']);
  finally
    FileVerInfo.Free;
  end;}
  AboutDialog := TAboutDialog.Create(nil);
  with AboutDialog do try
    LabelImgresGuiCpr.Caption := Text1;
    LabelImgresStr.Caption := Text2;
    LabelDependencies.Caption := Format(SCptDependenciesFmt, [laz_version, IntToStr(BGRABitmapVersion)]);

    MemoLicense.Text := License;
    ImageMainIcon.Picture.Icon := Application.Icon;
    result := ShowModal = mrOk;
  finally
    AboutDialog.Free;
  end;
end;

end.

