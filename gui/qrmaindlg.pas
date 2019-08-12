unit qrmaindlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ActnList, LCLType;

type

  { TMainDialog }

  TMainDialog = class(TForm)
    ActionExecute: TAction;
    ActionBrowseDstFilename: TAction;
    ActionBrowseSrcFilename: TAction;
    ActionList: TActionList;
    ButtonExecute: TBitBtn;
    ComboBoxSize: TComboBox;
    EditSrcFilename: TEdit;
    EditDstFilename: TEdit;
    ImageList24x24: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MemoMessages: TMemo;
    OpenDialogSrcFilenames: TOpenDialog;
    Panel1: TPanel;
    SaveDialogDstFilename: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure ActionBrowseDstFilenameExecute(Sender: TObject);
    procedure ActionBrowseSrcFilenameExecute(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FErrLine :string;
    procedure OnPrint(Sender :TObject; const Line :string);
  public

  end;

var
  MainDialog: TMainDialog;

implementation

uses
  imgres;

{$R *.lfm}

{ TMainDialog }

procedure TMainDialog.FormCreate(Sender: TObject);
var
  i :integer;
begin
  for i:=0 to High(DEFSIZES) do
    ComboBoxSize.Items.Add(IntToStr(DEFSIZES[i]));
end;

procedure TMainDialog.ActionBrowseSrcFilenameExecute(Sender: TObject);
begin
  if OpenDialogSrcFilenames.Execute then begin
    EditSrcFilename.Text := OpenDialogSrcFilenames.FileName;
  end;
end;

procedure TMainDialog.ActionExecuteExecute(Sender: TObject);
var
  ImgRes :TImgRes;
begin
  ImgRes := TImgRes.Create;
  with ImgRes do try
    StopOnError := true;
    SrcFilenames.Add(EditSrcFilename.Text);
    Sizes := ComboBoxSize.Text;
    DstFolder := ExtractFilePath(EditDstFilename.Text);
    OnPrint := @self.OnPrint;
    if not ImgRes.Execute then
      Application.MessageBox(PChar(FErrLine), PChar(Caption), MB_OK);
  finally
    ImgRes.Free;
  end;

end;

procedure TMainDialog.ActionBrowseDstFilenameExecute(Sender: TObject);
begin
  if SaveDialogDstFilename.Execute then begin
    EditDstFilename.Text := SaveDialogDstFilename.Filename;
  end;
end;

procedure TMainDialog.FormShow(Sender: TObject);
begin
  if ParamStr(1)<>'' then
    EditSrcFilename.Text := ParamStr(1);
end;

procedure TMainDialog.OnPrint(Sender: TObject; const Line: string);
begin
  MemoMessages.Lines.Add(Line);
end;

end.

