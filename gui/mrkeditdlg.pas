unit mrkeditdlg;

////////////////////////////////////////////////////////////////////////////////
// Watermark-Editor Inapp Application for ImageResize
// The dialog edits a .was file and creates a .png watermark image.

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ColorBox, ComCtrls, ExtCtrls, Buttons, ActnList, Menus, BGRABitmap,
  mrk.utils, IniFiles;

const
  WAMSINIEXT    = '.was';
  WAMSINITYP    = 'WAMS';
  WAMSINIVER100 = 100;
  WAMSINIVER    = 200;

type

  { TMrkEditDialog }

  TMrkEditDialog = class(TForm)
    ActionHelp: TAction;
    ActionNewDefault: TAction;
    ActionInsertCopyright: TAction;
    ActionSaveAsFavorite: TAction;
    ActionSave: TAction;
    ActionNew: TAction;
    ActionOpen: TAction;
    ActionFont: TAction;
    ActionSaveAs: TAction;
    ActionOk: TAction;
    ActionList1: TActionList;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    ButtonInsertCopyright: TBitBtn;
    ButtonBrowseFont: TBitBtn;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonSave: TBitBtn;
    CheckBoxShadow: TCheckBox;
    CheckBoxOutline: TCheckBox;
    ColorBoxFontColor: TColorBox;
    ColorBoxOutlineColor: TColorBox;
    ColorBoxShadowColor: TColorBox;
    ComboBoxShadowQuality: TComboBox;
    EditShadowBlur: TEdit;
    EditText: TEdit;
    EditFont: TEdit;
    FontDialog: TFontDialog;
    ImageLandscape: TImage;
    ImageList20x20: TImageList;
    ImageList32x32: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog: TOpenDialog;
    PaintBoxPreview: TPaintBox;
    PanelSettings: TPanel;
    PanelPreview: TPanel;
    PanelControls: TPanel;
    SaveDialog: TSaveDialog;
    ShapeWhite: TShape;
    ShapeBlack: TShape;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UpDownShadowBlur: TUpDown;
    procedure ActionHelpExecute(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionNewDefaultExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveAsFavoriteExecute(Sender: TObject);
    procedure ActionOkExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionFontExecute(Sender: TObject);
    procedure ButtonBrowseFontClick(Sender: TObject);
    procedure ButtonInsertCopyrightClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxPreviewPaint(Sender: TObject);
    procedure ShapeBlackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeWhiteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ShapeWhiteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure UpDownShadowBlurChanging(Sender: TObject; var AllowChange: Boolean      );
    procedure EditChanged(Sender :TObject);
  private
    FDirty :boolean;
    FMrkFilename :string;
    FMrkImage :TBGRABitmap;
    function GetSettingsFilename(const Title :string; CanCreate :boolean): string;
    function TryDialogToParams(out Params :TWatermarkParams) :boolean;
    procedure DialogToParams(out Params :TWatermarkParams);
    procedure ParamsToDialog(const Params :TWatermarkParams);
    function CreateMrkBitmap(var Img :TBGRABitmap) :boolean;
    procedure SaveTo(const SettingsRole :string);
    function LoadFrom(const SettingsRole :string) :boolean;
    procedure SaveToFile(const Filename :string);
    procedure SetDirty;
  public
    class function GetFilename(out MrkFilename :string) :boolean;
  end;

var
  MrkEditDialog: TMrkEditDialog;

implementation

uses
  imgres, graphics.utils, maindlg, windirs, helpintfs;

resourcestring
  SCptDialogTitle = 'Watermark Editor';
  SErrInvalidParams = 'Invalid parameters';
  SErrInvalidFileTypeFmt = 'Invalid filetype, %s expected.';
  SErrIncompatibleFileVersionFmt = 'Incompatible file version %.2f (%.2f expected).';

{$R *.lfm}

{ TMrkEditDialog }

procedure TMrkEditDialog.FormCreate(Sender: TObject);
begin
  Caption := SCptDialogTitle;
  FMrkImage := nil;
end;

procedure TMrkEditDialog.FormShow(Sender: TObject);
begin
  if not LoadFrom(WATERMARKLASTSETTINGS) then
    ActionNew.Execute;
end;

procedure TMrkEditDialog.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  SaveTo(WATERMARKLASTSETTINGS);
end;

procedure TMrkEditDialog.FormDestroy(Sender: TObject);
begin
  FMrkImage.Free;
end;

procedure TMrkEditDialog.ButtonSaveClick(Sender: TObject);
begin
  ActionSave.Execute;
end;

procedure TMrkEditDialog.ActionHelpExecute(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('','HTML/index.html#mrkeditor');
end;

function TMrkEditDialog.GetSettingsFilename(const Title :string; CanCreate :boolean): string;
begin
  result := IncludeTrailingPathDelimiter(GetWindowsSpecialDir(FOLDERID_LocalAppData, CanCreate))+ChangeFileExt(ExtractFileName(Application.ExeName), '.'+GUIVER_VERSION)+'\'+Title+'.was';
end;

procedure TMrkEditDialog.PaintBoxPreviewPaint(Sender: TObject);
var
  Fit :TRect;
begin
  if not Assigned(FMrkImage) then Exit;
  Fit := FitRect(TRect.Create(0, 0, FMrkImage.Width, FMrkImage.Height), PaintBoxPreview.ClientRect);
  FMrkImage.Draw(PaintBoxPreview.Canvas, Fit, false);
end;

procedure TMrkEditDialog.ShapeBlackMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PanelPreview.Color := clBlack;
  ImageLandscape.Visible := false;
end;

procedure TMrkEditDialog.ShapeWhiteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PanelPreview.Color := clWhite;
  ImageLandscape.Visible := false;
end;

procedure TMrkEditDialog.ShapeWhiteMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ImageLandscape.Visible := true;
end;

procedure TMrkEditDialog.UpDownShadowBlurChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
//  EditChanged(nil);
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
    Text := EditText.Text;
    if not TryStrToFontInfo(EditFont.Text, Params.FontSize, Params.FontName, Params.FontStyle) then Exit(false);
    FontColor := ColorBoxFontColor.Selected;
    Shadow := CheckBoxShadow.Checked;
    ShadowColor := ColorBoxShadowColor.Selected;
    ShadowBlur := UpDownShadowBlur.Position;
    ShadowQuality := ComboBoxShadowQuality.ItemIndex;
    Outline := CheckBoxOutline.Checked;
    OutlineColor := ColorBoxOutlineColor.Selected;
  end;
  result := true;
end;

procedure TMrkEditDialog.DialogToParams(out Params: TWatermarkParams);
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create(SErrInvalidParams);
end;

procedure TMrkEditDialog.ParamsToDialog(const Params: TWatermarkParams);
var
  OldDirty :boolean;
begin
  OldDirty := FDirty;
  with Params do begin
    EditText.Text := Text;
    EditFont.Text := FontInfoToStr(FontSize, FontName, FontStyle);
    ColorBoxFontColor.Selected := FontColor;
    CheckBoxShadow.Checked := Shadow;
    ColorBoxShadowColor.Selected := ShadowColor;
    UpDownShadowBlur.Position := ShadowBlur;
    ComboBoxShadowQuality.ItemIndex := ShadowQuality;
    CheckBoxOutline.Checked := Outline;
    ColorBoxOutlineColor.Selected := OutlineColor;
  end;
  FDirty := OldDirty;
end;

procedure TMrkEditDialog.ActionFontExecute(Sender: TObject);
var
  FontSize :integer;
  FontName :string;
  FontStyle :TFontStyles;
begin
  with FontDialog do begin
    if TryStrToFontInfo(EditFont.Text, FontSize, FontName, FontStyle) then begin
      Font.Name := FontName;
      Font.Style := FontStyle;
      Font.Size := FontSize;
    end;
    if Execute then begin
      EditFont.Text := FontInfoToStr(Font.Size, Font.Name, Font.Style);
    end;
  end;
end;

procedure TMrkEditDialog.ActionSaveAsFavoriteExecute(Sender: TObject);
var
  Ini: TIniFile;
  Params :TWatermarkParams;
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create(SErrInvalidParams);
  Ini := TIniFile.Create(GetSettingsFilename(WATERMARKFAVORITE, true));
  try
    Params.SaveToIni(Ini);
  finally
    Ini.Free;
  end;
  FDirty := false;
end;

procedure TMrkEditDialog.ActionNewExecute(Sender: TObject);
var
  Params :TWatermarkParams;
begin
  Params.Defaults;
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
      raise Exception.CreateFmt(SErrInvalidFileTypeFmt, [WAMSINITYP]);
    Ver := Ini.ReadInteger('Common', 'Version', 0);
    if Ver<>WAMSINIVER then
      raise Exception.CreateFmt(SErrIncompatibleFileVersionFmt, [Ver/100.0, WAMSINIVER/100.0]);
    PngFilename := Copy(OpenDialog.Filename, 1, Length(OpenDialog.Filename)-Length(WAMSINIEXT));
    Params.LoadFromIni(Ini);
    ParamsToDialog(Params);
    Caption := SCptDialogTitle + ' - ' + ExtractFilename(OpenDialog.Filename);
    FDirty := false;
    if FileExists(PngFilename) then begin
      FMrkFilename := PngFilename;
      ActionOk.Enabled := true;
    end;
  end;
end;

procedure TMrkEditDialog.ActionSaveAsExecute(Sender: TObject);
begin
  if SaveDialog.Execute then begin
    SaveToFile(SaveDialog.Filename);
    FDirty := false;
  end;
end;

procedure TMrkEditDialog.ButtonBrowseFontClick(Sender: TObject);
begin
  ActionFont.Execute;
end;

procedure TMrkEditDialog.ActionSaveExecute(Sender: TObject);
begin
  if FMrkFilename='' then
    ActionSaveAs.Execute
  else
    SaveToFile(FMrkFilename);
end;

procedure TMrkEditDialog.ButtonInsertCopyrightClick(Sender: TObject);
begin
  EditText.SelText := '©';
end;

procedure TMrkEditDialog.ActionNewDefaultExecute(Sender: TObject);
begin
  if not LoadFrom(WATERMARKFAVORITE) then
    ActionNew.Execute;
end;

function TMrkEditDialog.CreateMrkBitmap(var Img: TBGRABitmap): boolean;
var
  Params :TWatermarkParams;
begin
  Params.Defaults;
  result := TryDialogToParams(Params);
  if result then begin
    FreeAndNil(Img);
    result := TryCreateWatermarkImage(Params, Img);
  end;
end;

class function TMrkEditDialog.GetFilename(out MrkFilename: string): boolean;
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
      Caption := SCptDialogTitle + ' - ' + ExtractFilename(FMrkFilename);
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
    Caption := SCptDialogTitle + '*';
    FDirty := true;
    FMrkFilename := '';
    ActionOk.Enabled := false;
  end;
end;

function TMrkEditDialog.LoadFrom(const SettingsRole :string) :boolean;
var
  Ini :TIniFile;
  Params :TWatermarkParams;
  Filename :string;
begin
  Filename := GetSettingsFilename(SettingsRole, false);
  if not FileExists(Filename) then Exit(false);
  Ini := TIniFile.Create(Filename);
  try
    Params.LoadFromIni(Ini);
    ParamsToDialog(Params);
  finally
    Ini.Free;
  end;
  FDirty := false;
  result := true;
end;

procedure TMrkEditDialog.SaveTo(const SettingsRole :string);
var
  Ini: TIniFile;
  Params :TWatermarkParams;
begin
  if not TryDialogToParams(Params) then
    raise Exception.Create(SErrInvalidParams);
  Ini := TIniFile.Create(GetSettingsFilename(SettingsRole, true));
  try
    Params.SaveToIni(Ini);
  finally
    Ini.Free;
  end;
  FDirty := false;
end;

end.

