unit mrkeditdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ComCtrls, ExtCtrls, Buttons, ActnList, BGRABitmap,
  mrk.utils, IniFiles;

const
  DIALOGTITLE = 'Watermark Editor';
  WAMSINIEXT = '.was';
  WAMSINITYP = 'WAMS';
  WAMSINIVER = 100;

type

  { TMrkEditDialog }

  TMrkEditDialog = class(TForm)
    ActionDefault: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionFont: TAction;
    ActionSaveAs: TAction;
    ActionOk: TAction;
    ActionList1: TActionList;
    ButtonBrowseFont: TBitBtn;
    ButtonOkAndSave: TBitBtn;
    ButtonOkAndSave2: TBitBtn;
    ColorBoxFontColor: TColorBox;
    ColorBoxShadowColor: TColorBox;
    EditWidth: TEdit;
    EditShadowBlur: TEdit;
    EditText: TEdit;
    EditFont: TEdit;
    FontDialog: TFontDialog;
    ImageList20x20: TImageList;
    ImageList24x24: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    PaintBoxPreview: TPaintBox;
    PanelPreview: TPanel;
    SaveDialog: TSaveDialog;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton5: TToolButton;
    UpDownShadowBlur: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure ActionDefaultExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ButtonBrowseFontClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure UpDownShadowBlurChanging(Sender: TObject; var AllowChange: Boolean      );
    procedure EditChanged(Sender :TObject);
  private
    FMrkSource :integer;
    FMrkFilename :string;
    FMrkImage :TBGRABitmap;
    FDirty :boolean;
    function TryDialogToParams(out Params :TWatermarkParams) :boolean;
    procedure DialogToParams(out Params :TWatermarkParams);
    procedure ParamsToDialog(const Params :TWatermarkParams);
    function CreateMrkBitmap(var Img :TBGRABitmap) :boolean;
    procedure SaveToRegistry;
    procedure LoadFromRegistry;
    procedure SaveToFile(const Filename :string);
    procedure SetDirty;
  public
    class function GetFilename(out MrkFilename :string) :boolean;
  end;

var
  MrkEditDialog: TMrkEditDialog;

implementation

uses
  imgres, graphics.utils;

const
  DLGREGKEY = REGKEY + '\MrkEditor';

{$R *.lfm}

{ TMrkEditDialog }

procedure TMrkEditDialog.PaintBoxPreviewPaint(Sender: TObject);
var
  Fit :TRect;
begin
  if not Assigned(FMrkImage) then Exit;
  Fit := FitRect(TRect.Create(0, 0, FMrkImage.Width, FMrkImage.Height), PaintBoxPreview.ClientRect);
  FMrkImage.Draw(PaintBoxPreview.Canvas, Fit, false);
end;

procedure TMrkEditDialog.UpDownShadowBlurChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  EditChanged(nil);
end;

procedure TMrkEditDialog.EditChanged(Sender: TObject);
begin
  SetDirty;
  if CreateMrkBitmap(FMrkImage) then
    PaintBoxPreview.Invalidate;
end;

function TMrkEditDialog.TryDialogToParams(out Params: TWatermarkParams): boolean;
begin
  with Params do begin
    if not TryStrToInt(EditWidth.Text, Width) then Exit(false);
    Text := EditText.Text;
    if not TryStrToFontInfo(EditFont.Text, Params.FontName, Params.FontStyle) then Exit(false);
    FontColor := ColorBoxFontColor.Selected;
    ShadowColor := ColorBoxShadowColor.Selected;
    ShadowBlur := UpDownShadowBlur.Position;
  end;
  result := true;
end;

procedure TMrkEditDialog.DialogToParams(out Params: TWatermarkParams);
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create('Invalid parameters');
end;

procedure TMrkEditDialog.ParamsToDialog(const Params: TWatermarkParams);
var
  OldDirty :boolean;
begin
  OldDirty := FDirty;
  with Params do begin
    EditWidth.Text := IntToStr(Width);
    EditText.Text := Text;
    EditFont.Text := FontInfoToStr(FontName, FontStyle);
    ColorBoxFontColor.Selected := FontColor;
    ColorBoxShadowColor.Selected := ShadowColor;
    UpDownShadowBlur.Position := ShadowBlur;
  end;
  FDirty := OldDirty;
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

procedure TMrkEditDialog.ActionDefaultExecute(Sender: TObject);
var
  Ini: TRegistryIniFile;
  Params :TWatermarkParams;
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create('Invalid parameter.');
  Ini := TRegistryIniFile.Create(DLGREGKEY);
  try
    Params.SaveToIni(Ini, WATERMARKDEFAULTSECTION);
  finally
    Ini.Free;
  end;
  FDirty := false;
end;

procedure TMrkEditDialog.ActionNewExecute(Sender: TObject);
var
  Ini: TRegistryIniFile;
  Params :TWatermarkParams;
begin
  Ini := TRegistryIniFile.Create(REGKEY);
  try
    if Ini.RegIniFile.KeyExists(WATERMARKDEFAULTSECTION) then
      Params.LoadFromIni(Ini, WATERMARKDEFAULTSECTION)
    else
      Params.Defaults;
  finally
    Ini.Free;
  end;
  ParamsToDialog(Params);
  FDirty := false;
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

procedure TMrkEditDialog.ActionOpenExecute(Sender: TObject);
var
  Params :TWatermarkParams;
  Ini :TIniFile;
  Ver :integer;
  PngFilename :string;
begin
  if OpenDialog.Execute then begin
    Params.Defaults;
    DialogToParams(Params);
    Ini := TIniFile.Create(OpenDialog.Filename);
    if Ini.ReadString('Common', 'Type', '')<>WAMSINITYP then
      raise Exception.CreateFmt('Invalid filetype, %s expected.', [WAMSINITYP]);
    Ver := Ini.ReadInteger('Common', 'Version', 0);
    if Ver<>WAMSINIVER then
      raise Exception.CreateFmt('Incompatible file version %.2f, %.2f expected.', [Ver, WAMSINITYP]);
    PngFilename := Copy(OpenDialog.Filename, 1, Length(OpenDialog.Filename)-Length(WAMSINIEXT));
    Params.LoadFromIni(Ini);
    ParamsToDialog(Params);
    Caption := DIALOGTITLE + ' - ' + ExtractFilename(OpenDialog.Filename);
    FDirty := false;
    if FileExists(PngFilename) then begin
      FMrkFilename := PngFilename;
      ActionOk.Enabled := true;
    end;
  end;
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
  FMrkImage := nil;
  LoadFromRegistry;
end;

procedure TMrkEditDialog.FormDestroy(Sender: TObject);
begin
  FMrkImage.Free;
end;

function TMrkEditDialog.CreateMrkBitmap(var Img: TBGRABitmap): boolean;
var
  Params :TWatermarkParams;
begin
  Params.Defaults;
  result := TryDialogToParams(Params)
    and TryCreateWatermarkImage(Params, Img);
end;

class function TMrkEditDialog.GetFilename(out MrkFilename: string): boolean;
var
  Dialog :TMrkEditDialog;
begin
  Dialog := TMrkEditDialog.Create(Application);
  try
    Dialog.FMrkSource := msFile;
    result := Dialog.ShowModal=mrOk;
    if result then
      MrkFilename := Dialog.FMrkFilename;
  finally
    Dialog.Free;
  end;
end;

procedure TMrkEditDialog.LoadFromRegistry;
var
  Ini :TRegistryIniFile;
  Params :TWatermarkParams;
begin
  Ini := TRegistryIniFile.Create(REGKEY);
  try
    Ini.RegIniFile.RootKey := HKEY_CURRENT_USER;
    Params.LoadFromIni(Ini);
    ParamsToDialog(Params);
  finally
    Ini.Free;
  end;
  FDirty := false;
end;

procedure TMrkEditDialog.SaveToFile(const Filename: string);
var
  Img :TBGRABitmap;
  Params :TWatermarkParams;
  Ini :TIniFile;
begin
  Img := nil;
  Ini := nil;
  try
    DialogToParams(Params);
    if CreateMrkBitmap(Img) then begin
      FMrkFilename := Filename;
      ActionOk.Enabled := true;
      Img.SaveToFile(FMrkFilename);
      Ini := TIniFile.Create(FMrkFilename + WAMSINIEXT);
      Ini.WriteString('Common', 'Type', WAMSINITYP);
      Ini.WriteInteger('Common', 'Version', WAMSINIVER);
      Params.SaveToIni(Ini);
      Caption := DIALOGTITLE + ' - ' + ExtractFilename(FMrkFilename);
      FDirty := false;
    end;
  finally
    Img.Free;
    Ini.Free;
  end;
end;

procedure TMrkEditDialog.SetDirty;
begin
  if not FDirty then begin
    Caption := DIALOGTITLE + '*';
    FDirty := true;
    FMrkFilename := '';
    ActionOk.Enabled := false;
  end;
end;

procedure TMrkEditDialog.SaveToRegistry;
var
  Ini: TRegistryIniFile;
  Params :TWatermarkParams;
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create('Invalid parameter.');
  Ini := TRegistryIniFile.Create(REGKEY);
  try
    Params.SaveToIni(Ini);
  finally
    Ini.Free;
  end;
  FDirty := false;
end;

end.

