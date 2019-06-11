unit mrkeditdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ComCtrls, ExtCtrls, Buttons, ActnList, BGRABitmap;

const
  DIALOGTITLE = 'Watermark Editor';

type

  { TMrkEditDialog }

  TMrkEditDialog = class(TForm)
    ActionFont: TAction;
    ActionSaveAs: TAction;
    ActionOk: TAction;
    ActionList1: TActionList;
    ButtonBrowseFont: TBitBtn;
    ButtonOkAndSave: TBitBtn;
    ButtonOkAndSave1: TBitBtn;
    ButtonOkAndSave2: TBitBtn;
    ColorBoxFontColor: TColorBox;
    ColorBoxShadowColor: TColorBox;
    EditWidth: TEdit;
    EditShadowBlur: TEdit;
    EditText: TEdit;
    EditFont: TEdit;
    FontDialog: TFontDialog;
    ImageList20x20: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PaintBoxPreview: TPaintBox;
    PanelPreview: TPanel;
    SaveDialog: TSaveDialog;
    UpDownShadowBlur: TUpDown;
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ButtonBrowseFontClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure UpDownShadowBlurChanging(Sender: TObject; var AllowChange: Boolean
      );
    procedure EditChanged(Sender :TObject);

  private
    FMrkFilename :string;
    FDirty :boolean;
    function CreateMrkBitmap(Out Img :TBGRABitmap) :boolean;
    procedure SaveToRegistry;
    function LoadFromRegistry :boolean;
    procedure SaveToFile(const Filename :string);
  public
    class function Execute(out MrkFilename :string) :boolean;
  end;

var
  MrkEditDialog: TMrkEditDialog;

implementation

uses
  mrk.utils, imgres;

const
  DLGREGKEY = REGKEY + '\MrkEditor';

{$R *.lfm}

{ TMrkEditDialog }

procedure TMrkEditDialog.PaintBoxPreviewPaint(Sender: TObject);
var
  Img :TBGRABitmap;
begin
  if CreateMrkBitmap(Img) then begin
    Img.Draw(PaintBoxPreview.Canvas, 0, 0, false);
    Img.Free;
  end;
end;

procedure TMrkEditDialog.UpDownShadowBlurChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  EditChanged(nil);
end;

procedure TMrkEditDialog.EditChanged(Sender: TObject);
begin
  FDirty := true;
  PaintBoxPreview.Invalidate;
end;

procedure TMrkEditDialog.ActionFontExecute(Sender: TObject);
var
  FontName :string;
  FontStyle :TFontStyles;
begin
  with FontDialog do begin
    if TryStrToFontInfo(EditFont.Text, FontName, FontStyle) then begin
      Font.Name := FontName;
      Font.Style := FontStyle;
      Font.Size := 20;
    end;
    if Execute then begin
      EditFont.Text := FontInfoToStr(Font.Name, Font.Style);
    end;
  end;
end;

procedure TMrkEditDialog.ActionOkExecute(Sender: TObject);
begin
  if FDirty then begin
    if SaveDialog.Execute then begin
      SaveToFile(SaveDialog.Filename);
      ModalResult := mrOk;
    end;
  end else
    ModalResult := mrOk;
end;

procedure TMrkEditDialog.ActionSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then
    SaveToFile(SaveDialog.Filename);
end;

procedure TMrkEditDialog.ButtonBrowseFontClick(Sender: TObject);
begin
  ActionFont.Execute;
end;

procedure TMrkEditDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveToRegistry;
end;

procedure TMrkEditDialog.FormCreate(Sender: TObject);
begin
  Caption := DIALOGTITLE;
  LoadFromRegistry;
end;

function TMrkEditDialog.CreateMrkBitmap(out Img: TBGRABitmap): boolean;
var
  Params :TWatermarkImageParams;
begin
  with Params do begin
    Width := PaintBoxPreview.Width;//StrToInt(EditWidth.Text);
    Text := EditText.Text;
    if not TryStrToFontInfo(EditFont.Text, Params.FontName, Params.FontStyle) then Exit(false);
    Params.FontColor := ColorBoxFontColor.Selected;
    Params.ShadowColor := ColorBoxShadowColor.Selected;
    Params.ShadowBlur := UpDownShadowBlur.Position;
    result := TryCreateWatermarkImage(Params, Img);
  end;
end;

class function TMrkEditDialog.Execute(out MrkFilename: string): boolean;
var
  Dialog :TMrkEditDialog;
begin
  Dialog := TMrkEditDialog.Create(Application);
  try
    result := Dialog.ShowModal=mrOk;
    if result then
      MrkFilename := Dialog.FMrkFilename;
  finally
    Dialog.Free;
  end;
end;

function TMrkEditDialog.LoadFromRegistry :boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    // Navigate to proper "directory":
    with Registry do begin
      result := OpenKey(DLGREGKEY, false);
      if result then begin
        EditWidth.Text := ReadString('Size');
        EditText.Text := ReadString('Text');
        EditFont.Text := ReadString('Font');
        ColorBoxFontColor.Selected := ReadInteger('FontColor');
        ColorBoxShadowColor.Selected := ReadInteger('ShadowColor');
        UpDownShadowBlur.Position := ReadInteger('ShadowBlur');
      end;
    end;
  finally
    Registry.Free;
  end;
  FDirty := false;
end;

procedure TMrkEditDialog.SaveToFile(const Filename: string);
var
  Img :TBGRABitmap;
begin
  Img := nil;
  try
    if CreateMrkBitmap(Img) then begin
      FMrkFilename := Filename;
      Img.SaveToFile(FMrkFilename);
      Caption := DIALOGTITLE + ' - ' + ExtractFilename(FMrkFilename);
      FDirty := false;
    end;
  finally
    Img.Free;
  end;
end;

procedure TMrkEditDialog.SaveToRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    if Registry.OpenKey(DLGREGKEY, true) then with Registry do begin
      WriteString('Size', EditWidth.Text);
      WriteString('Text', EditText.Text);
      WriteString('Font', EditFont.Text);
      WriteInteger('FontColor', ColorBoxFontColor.Selected);
      WriteInteger('ShadowColor', ColorBoxShadowColor.Selected);
      WriteInteger('ShadowBlur', UpDownShadowBlur.Position);
    end;
  finally
    Registry.CloseKey;
    Registry.Free;
  end;
  FDirty := false;
end;

end.

