unit maindlg;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2019 Jan Schirrmacher, www.atomek.de
//
//  See https://github.com/Atomek61/ImageResize.git for licensing
//
//  maindlg.pas is the GUI interface for the TImgRes-Processor.
//
////////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, ExtCtrls, imgres, registry, aboutdlg, inifiles, strutils,
  LMessages, LCLIntf, Buttons, ImgList, LCLType, LazHelpHTML,
  BGRABitmap, BGRABitmapTypes, Generics.Collections;

const
  WEBHELPURL = 'http://www.atomek.de/imageresize/hlp23/gui/';

  IMGRESGUIVER = '2.4';
  IMGRESGUICPR = 'ImageResize V'+IMGRESGUIVER+' © 2019 Jan Schirrmacher, www.atomek.de';

  INITYPE = 'IRS';
  INIVERSION = '200';

  GUIREGKEY = IMGRESREGKEY+IMGRESGUIVER+'\';

  COMMONSECTION = 'Common';
  SETTINGSSECTION = 'Settings';

  MRKRECTRATIO = 3.0;

  LINESEP = '|';

  LICENSE =
    'Image Resize Copyright (c) 2019 Jan Schirrmacher, www.atomek.de'#10#10+
    'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify and merge copies of the Software, subject to the following conditions:'#10#10+
    'The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.'#10#10+
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';
  WEBURL = 'www.atomek.de/imageresize/index.html';


  LM_RUN = LM_USER + 1;

  RENSIMPLETEMPLATE = 'img%INDEX:1,3%.%FILEEXT%';
  RENADVANCEDTEMPLATE = 'img%INDEX:1,3%_%SIZE%.%FILEEXT%';

  THUMBNAILIMGMAX = 240;
  DOCIMGMAX = 960;

  SIZEBTNHINTFMT = '%s - %dpx';

type

  { TMainDialog }

  TSizeDict = specialize TDictionary<integer, integer>;

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TMainDialog = class(TForm)
    ActionEditWatermark: TAction;
    ActionHelp: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    ActionOpen: TAction;
    ActionNew: TAction;
    ActionClearSizes: TAction;
    ActionBrowseMrkFilename: TAction;
    ActionAbout: TAction;
    ActionBrowseDstFolder: TAction;
    ActionBrowseFilenames: TAction;
    ActionClearFilenames: TAction;
    ActionExecute: TAction;
    ActionList: TActionList;
    ApplicationProperties1: TApplicationProperties;
    ButtonAbout: TBitBtn;
    ButtonBrowseMrkFilename: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonClearSizes2: TBitBtn;
    ButtonClearSrcFiles: TBitBtn;
    ButtonBrowseSrcFiles: TBitBtn;
    ButtonMrkEdit: TBitBtn;
    ButtonExecute: TBitBtn;
    BrowseDstFolder: TSelectDirectoryDialog;
    CheckBoxRenEnabled: TCheckBox;
    CheckBoxMrkEnabled: TCheckBox;
    CheckBoxStopOnError: TCheckBox;
    EditRenTemplate: TComboBox;
    ComboBoxBoost: TComboBox;
    ComboBoxJpgQuality: TComboBox;
    ComboBoxPngCompression: TComboBox;
    EditMrkAlpha: TEdit;
    EditMrkFilename: TEdit;
    EditMrkSize: TEdit;
    EditMrkX: TEdit;
    EditMrkY: TEdit;
    EditSizes: TEdit;
    EditDstFolder: TEdit;
    GroupBoxRename: TGroupBox;
    GroupBoxMrkLayout: TGroupBox;
    GroupBoxJpgOptions: TGroupBox;
    GroupBoxPngOptions: TGroupBox;
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
    ImageListMrkPositions: TImageList;
    ImageList20x20: TImageList;
    ImageList24x24: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    LabelCores: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    LabelMrkSpace: TLabel;
    LabelSizesRequired: TLabel;
    LabelSrcFilnamesRequired: TLabel;
    LabelDstFolderRequired: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    MemoMessages: TMemo;
    MemoSrcFilenames: TMemo;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PageControl: TPageControl;
    PaintBox1: TPaintBox;
    PaintBoxMrkPreview: TPaintBox;
    PanelMrkSourceFile: TPanel;
    PanelMessages: TPanel;
    PanelControls: TPanel;
    PanelMrkSourceImage: TPanel;
    PanelPreview: TPanel;
    ProgressBar: TProgressBar;
    RadioButtonRenSimple: TRadioButton;
    RadioButtonRenCustom: TRadioButton;
    RadioButtonRenAdvanced: TRadioButton;
    SaveAsDialog: TSaveDialog;
    Splitter1: TSplitter;
    TabSheetRename: TTabSheet;
    TabSheetMrk: TTabSheet;
    TabSheetRessources: TTabSheet;
    TabSheetSizes: TTabSheet;
    TabSheetQuality: TTabSheet;
    TimerProgressBarOff: TTimer;
    ToolBar: TToolBar;
    ToolBarSizeButtons: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    UpDownMrkAlpha: TUpDown;
    UpDownMrkSize: TUpDown;
    UpDownMrkX: TUpDown;
    UpDownMrkY: TUpDown;
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure CheckBoxRenEnabledClick(Sender: TObject);
    procedure EditRenTemplateEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionBrowseDstFolderExecute(Sender: TObject);
    procedure ActionBrowseFilenamesExecute(Sender: TObject);
    procedure ActionBrowseMrkFilenameExecute(Sender: TObject);
    procedure ActionClearFilenamesExecute(Sender: TObject);
    procedure ActionClearSizesExecute(Sender: TObject);
    procedure ActionEditWatermarkExecute(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure EditDstFolderChange(Sender: TObject);
    procedure EditMrkSizeChange(Sender: TObject);
    procedure EditSizesChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure MemoSrcFilenamesChange(Sender: TObject);
    procedure PaintBoxMrkPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMrkPreviewMouseLeave(Sender: TObject);
    procedure PaintBoxMrkPreviewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMrkPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMrkPreviewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMrkPreviewPaint(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
    procedure CheckBoxMrkEnabledChange(Sender: TObject);
    procedure EditSizesExit(Sender: TObject);
  private
    FAutoExit :boolean;
    FIsSave :boolean;
    FIniFilename :string;
    FImgRes :TImgRes;
    FCancelled :boolean;
    FExecuting :boolean;
    FProgress :single;
    FMrkSource :integer;
    FMrkDragging :boolean;
    FMrkOffset :TSize; // while dragging
    procedure SetTitle(const Str :string);
    procedure SetMrkSource(Value :integer);
    function GetMrkSource :integer;
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnProgress(Sender :TObject; Progress :single);
    function LoadFromIni(Ini :TCustomIniFile) :boolean;
    procedure SaveToIni(Ini :TCustomIniFile);
    function LoadFromRegistry :boolean;
    procedure SaveToRegistry;
    function LoadFromFile(const Filename :string) :boolean;
    procedure SaveToFile(const Filename :string);
    procedure Log(const Msg :string);
    function BoostStrToThreadCount(const Value :string) :integer;
    function ThreadCountToBoostStr(ThreadCount :integer) :string;
    procedure UpdateSizes;
    procedure UpdateControls;
    procedure LMRun(var Message: TLMessage); message LM_RUN;
    procedure SizeButtonClick(Sender :TObject);
    function MouseToSpace(X, Y :integer) :TSize;
    function MouseToMrkSpace(X, Y :integer; out Value :TSize) :boolean;
    function CalcMarkRect(out Rect :TRect) :boolean;
  public

  end;

var
  MainDialog: TMainDialog;

implementation

uses
  math, mrkeditdlg, helpintfs, Windows;

{$R *.lfm}

{ TPos }

constructor TPos.Create(ax, ay: single);
begin
  self.x := ax;
  self.y := ay;
end;

{ TMainDialog }

procedure TMainDialog.FormCreate(Sender: TObject);
var
  i :integer;
  Button :TToolButton;
  Cpt :string;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  AllowDropFiles := true;

  // Create Size Buttons
  for i:=0 to High(DEFSIZES) do begin
    Button := TToolButton.Create(ToolBarSizeButtons);
    Button.Parent := ToolBarSizeButtons;
    Button.Caption := IntToStr(DEFSIZES[i]);
    if DEFSIZES[i]<=THUMBNAILIMGMAX then
      Cpt := 'Small - Thumbnail size'
    else if DEFSIZES[i]<=DOCIMGMAX then
      Cpt := 'Medium - Document size'
    else
      Cpt := 'Large - Desktop size';

    Button.Hint := Format(SIZEBTNHINTFMT, [Cpt, DEFSIZES[i]]);
    if DEFSIZES[i]<300 then
        Button.ImageIndex := 5
    else if DEFSIZES[i]<1000 then
      Button.ImageIndex := 6
    else
      Button.ImageIndex := 7;
    Button.Style := tbsCheck;
    Button.Tag := DEFSIZES[i];
    Button.OnClick := @SizeButtonClick;
    Button.Visible := true;
  end;
  ToolBarSizeButtons.ButtonWidth := ToolBarSizeButtons.Width div 4;
  ToolBarSizeButtons.ButtonHeight := ToolBarSizeButtons.Height div 4;

end;

procedure TMainDialog.FormShow(Sender: TObject);
var
  Params :TStringArray;
begin
  PanelMrkSourceImage.Left := PanelMrkSourceFile.Left;
  PanelMrkSourceImage.Top := PanelMrkSourceFile.Top;
  PanelMrkSourceImage.Width := PanelMrkSourceFile.Width;
  if not LoadFromRegistry then
    ActionNew.Execute;
  UpdateControls;
  FAutoExit := Application.HasOption('X', 'AUTOEXIT');
  Params := Application.GetNonOptions('AX', ['AUTOSTART', 'AUTOEXIT']);
  if Length(Params)=1 then
    LoadFromFile(Params[0]);
  if Application.HasOption('A', 'AUTOSTART') then
    PostMessage(Handle, LM_RUN, 0, 0);

  PageControl.ActivePageIndex := 0;

  // Show number of cores
  LabelCores.Caption := Format('%d Cores', [TThread.ProcessorCount]);

  // If no local help files, then use online help
  if not FileExists(IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+'hlp/gui/index.html') then
    HTMLHelpDatabase.BaseURL := WEBHELPURL;
end;

procedure TMainDialog.LMRun(var Message: TLMessage);
begin
  ActionExecute.Execute;
  Application.ProcessMessages;
  if FAutoExit then begin
    Close;
  end;
end;

procedure TMainDialog.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log('Error - '+E.Message);
end;

procedure TMainDialog.CheckBoxRenEnabledClick(Sender: TObject);
begin
  GroupBoxRename.Visible := CheckBoxRenEnabled.Checked;
end;

procedure TMainDialog.EditRenTemplateEnter(Sender: TObject);
begin
  RadioButtonRenCustom.Checked := true;
end;

procedure TMainDialog.CheckBoxMrkEnabledChange(Sender: TObject);
const
  MRKSOURCES :array[boolean] of integer = (msDisabled, msFile);
begin
  SetMrkSource(MRKSOURCES[CheckBoxMrkEnabled.Checked]);
end;

procedure TMainDialog.EditSizesExit(Sender: TObject);
begin
  UpdateSizes;
end;

function TMainDialog.LoadFromIni(Ini :TCustomIniFile) :boolean;
var
  IniVer :string;
  IniTyp :string;
begin
  with Ini do begin
    IniTyp := Ini.ReadString('Common', 'Type', 'unknown');
    result := IniTyp=INITYPE;
    if not result then begin
       Log('Cant load Image Resize Settings File');
       Exit;
    end;
    IniVer := Ini.ReadString('Common', 'Version', '000');
    result := IniVer=INIVERSION;
    if not result then begin
      Log(Format('Warning: Unexpected format %s (%s expected).', [IniVer, INIVERSION]));
      Exit;
    end;
    MemoSrcFilenames.Text := ReplaceStr(ReadString(SETTINGSSECTION, 'SrcFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP)), LINESEP, #13#10);
    EditDstFolder.Text := ReadString(SETTINGSSECTION, 'DstFolder', EditDstFolder.Text);
    EditSizes.Text := ReadString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    UpdateSizes;
    ComboBoxJpgQuality.Text := ReadString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    ComboBoxPngCompression.Text := ReadString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    SetMrkSource(ReadInteger(SETTINGSSECTION, 'MrkSource', GetMrkSource));
    EditMrkFilename.Text := ReadString(SETTINGSSECTION, 'MrkFilename', EditMrkFilename.Text);
    UpDownMrkSize.Position := ReadInteger(SETTINGSSECTION, 'MrkSize', UpDownMrkSize.Position);
    UpDownMrkX.Position := ReadInteger(SETTINGSSECTION, 'MrkX', UpDownMrkX.Position);
    UpDownMrkY.Position := ReadInteger(SETTINGSSECTION, 'MrkY', UpDownMrkY.Position);
    UpDownMrkAlpha.Position := ReadInteger(SETTINGSSECTION, 'MrkAlpha', UpDownMrkAlpha.Position);
    ComboBoxBoost.Text := ReadString(SETTINGSSECTION, 'ThreadCount', ComboBoxBoost.Text);
    CheckBoxStopOnError.Checked := ReadBool(SETTINGSSECTION, 'StopOnError', CheckBoxStopOnError.Checked);
    CheckBoxRenEnabled.Checked := ReadBool(SETTINGSSECTION, 'RenEnabled', CheckBoxRenEnabled.Checked);
    RadioButtonRenSimple.Checked := ReadBool(SETTINGSSECTION, 'RenSimple', RadioButtonRenSimple.Checked);
    RadioButtonRenAdvanced.Checked := ReadBool(SETTINGSSECTION, 'RenAdvanced', RadioButtonRenAdvanced.Checked);
    RadioButtonRenCustom.Checked := ReadBool(SETTINGSSECTION, 'RenCustom', RadioButtonRenCustom.Checked);
    EditRenTemplate.Text := ReadString(SETTINGSSECTION, 'RenTemplate', EditRenTemplate.Text);
  end;
end;

procedure TMainDialog.SaveToIni(Ini :TCustomIniFile);
begin
  // Navigate to proper "directory":
  with Ini do begin
    WriteString(COMMONSECTION, 'Type', INITYPE);
    WriteString(COMMONSECTION, 'Version', INIVERSION);
    EraseSection(SETTINGSSECTION);
    WriteString(SETTINGSSECTION, 'SrcFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP));
    WriteString(SETTINGSSECTION, 'DstFolder', EditDstFolder.Text);
    WriteString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    WriteString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    WriteString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    WriteInteger(SETTINGSSECTION, 'MrkSource', GetMrkSource);
    WriteString(SETTINGSSECTION, 'MrkFilename', EditMrkFilename.Text);
    WriteString(SETTINGSSECTION, 'MrkSize', EditMrkSize.Text);
    WriteString(SETTINGSSECTION, 'MrkX', EditMrkX.Text);
    WriteString(SETTINGSSECTION, 'MrkY', EditMrkY.Text);
    WriteString(SETTINGSSECTION, 'MrkAlpha', EditMrkAlpha.Text);
    WriteString(SETTINGSSECTION, 'ThreadCount', ComboBoxBoost.Text);
    WriteBool(SETTINGSSECTION, 'StopOnError', CheckBoxStopOnError.Checked);
    WriteBool(SETTINGSSECTION, 'RenEnabled', CheckBoxRenEnabled.Checked);
    WriteBool(SETTINGSSECTION, 'RenSimple', RadioButtonRenSimple.Checked);
    WriteBool(SETTINGSSECTION, 'RenAdvanced', RadioButtonRenAdvanced.Checked);
    WriteBool(SETTINGSSECTION, 'RenCustom', RadioButtonRenCustom.Checked);
    WriteString(SETTINGSSECTION, 'RenTemplate', EditRenTemplate.Text);
  end;
end;

function TMainDialog.LoadFromRegistry: boolean;
var
  Ini :TRegistryIniFile;
begin
  Ini := TRegistryIniFile.Create(GUIREGKEY, KEY_READ);
  try
    result := LoadFromIni(Ini);
  finally
     Ini.Free;
  end;
  SetTitle('last settings');
end;

procedure TMainDialog.SaveToRegistry;
var
  Ini :TRegistryIniFile;
begin
  Ini := TRegistryIniFile.Create(GUIREGKEY, KEY_WRITE);
  try
    SaveToIni(Ini);
  finally
    Ini.Free;
  end;
end;

function TMainDialog.LoadFromFile(const Filename: string) :boolean;
var
  Ini :TIniFile;
begin
  if not FileExists(Filename) then
    raise Exception.CreateFmt('File ''%s'' not found.', [Filename]);
  Ini := TIniFile.Create(Filename);
  try
    result := LoadFromIni(Ini);
    if result then begin
      FIsSave := true;
      FIniFilename := Filename;
      SetTitle(''''+Filename+'''');
    end;
  finally
     Ini.Free;
  end;
end;

procedure TMainDialog.SaveToFile(const Filename: string);
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  try
    SaveToIni(Ini);
    FIsSave := true;
    FIniFilename := Filename;
    SetTitle(''''+FIniFilename+'''');
    Log(Format('Settings saved to ''%s''', [FIniFilename]));
  finally
    Ini.Free;
  end;
end;

procedure TMainDialog.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  MemoSrcFilenames.Lines.AddStrings(Filenames);
end;

procedure TMainDialog.SizeButtonClick(Sender: TObject);
var
  SizeStr :string;
  Sizes :TSizes;
  Button :TToolButton;
  i, j :integer;
begin
  Button := Sender as TToolButton;
  if Button.Down then begin
    SizeStr := IntToStr(Button.Tag);
    if Length(Trim(EditSizes.Text))=0 then
      EditSizes.Text := SizeStr
    else
      EditSizes.Text := EditSizes.Text + ', ' + SizeStr;
  end else begin
    if TrySizesStrToSizes(EditSizes.Text, Sizes) then begin
      for i:=0 to High(Sizes) do
        if Sizes[i]=Button.Tag then begin
          for j:=i to Length(Sizes)-2 do
            Sizes[j] := Sizes[j+1];
          SetLength(Sizes, Length(Sizes)-1);
          EditSizes.Text := SizesToSizesStr(Sizes);
          break;
        end;
    end;
  end;
  UpdateSizes;
  UpdateControls;
end;

procedure TMainDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveToRegistry;
end;

procedure TMainDialog.ActionNewExecute(Sender: TObject);
var
  ImgResizer :TImgRes;
begin
  MemoMessages.Lines.Clear;
  ImgResizer := TImgRes.Create;
  try
    MemoSrcFilenames.Lines.Clear;
    EditDstFolder.Text := '';
    EditSizes.Text := '';
    ComboBoxJpgQuality.Text := ImgResizer.JpgQualityToStr(ImgResizer.JpgQuality);
    ComboBoxPngCompression.Text := TImgRes.PngCompressionToStr(ImgResizer.PngCompression);
    EditMrkFilename.Text := '';
    UpDownMrkSize.Position := round(ImgResizer.MrkSize);
    UpDownMrkX.Position := round(ImgResizer.MrkX);
    UpDownMrkY.Position := round(ImgResizer.MrkY);
    UpDownMrkAlpha.Position := round(ImgResizer.MrkAlpha);
    FIsSave := false;
    FIniFilename := '';
    CheckBoxRenEnabled.Checked := ImgResizer.RenEnabled;
    RadioButtonRenSimple.Checked := true;
    EditRenTemplate.Text := DEFAULT_RENFILETEMPLATE;
    SetMrkSource(msDisabled);
    UpdateSizes;
    UpdateControls;
    PageControl.ActivePage := TabSheetSizes;
    SetTitle('unnamed');
  finally
    ImgResizer.Free;
  end;
end;

procedure TMainDialog.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    LoadFromFile(OpenDialog.Filename);
  end;
end;

procedure TMainDialog.Log(const Msg: string);
begin
  MemoMessages.Lines.Add(Msg);
end;

function TMainDialog.BoostStrToThreadCount(const Value: string): integer;
var
  s :string;
begin
  s := LowerCase(Trim(Value));
  if s='single' then
    result := 1
  else if s='maximum' then
    result := 0
  else
    result := StrToInt(s);
end;

function TMainDialog.ThreadCountToBoostStr(ThreadCount: integer): string;
begin
  case ThreadCount of
  0:
    result := 'maximum';
  1:
    result := 'single';
  else
    result := IntToStr(ThreadCount);
  end;
end;

procedure TMainDialog.UpdateSizes;
var
  i :integer;
  SizeDict :TSizeDict;
  Sizes :TSizes;
begin
  if TrySizesStrToSizes(EditSizes.Text, Sizes) then begin
    EditSizes.Text := SizesToSizesStr(Sizes);
    SizeDict := TSizeDict.Create;
    try
      for i:=0 to High(Sizes) do
        SizeDict.Add(Sizes[i], i);
      with ToolBarSizeButtons do for i:=0 to ButtonCount-1 do
        Buttons[i].Down := SizeDict.ContainsKey(Buttons[i].Tag);
    finally
      SizeDict.Free;
    end;
  end;
end;

procedure TMainDialog.UpdateControls;
begin
  LabelSrcFilnamesRequired.Enabled := Length(MemoSrcFilenames.Text) = 0;
  LabelDstFolderRequired.Enabled := Length(EditDstFolder.Text) = 0;
  LabelSizesRequired.Enabled := Length(EditSizes.Text) = 0;
end;

procedure TMainDialog.ActionSaveExecute(Sender: TObject);
begin
  if not FIsSave then
    ActionSaveAs.Execute
  else
    SaveToFile(FIniFilename);
end;

procedure TMainDialog.ButtonExecuteClick(Sender: TObject);
begin
  ActionExecute.Execute;
end;

procedure TMainDialog.MemoSrcFilenamesChange(Sender: TObject);
begin
  LabelSrcFilnamesRequired.Enabled := Length(MemoSrcFilenames.Text) = 0;
end;

function TMainDialog.MouseToSpace(X, Y: integer): TSize;
var
  sx :single;
  sy :single;
  r :TRect;
begin
  r := PaintBoxMrkPreview.ClientRect;
  if X<r.Left then X := r.Left else if X>r.Right then X := r.Right;
  if Y<r.Top then Y := r.Top else if Y>r.Bottom then Y := r.Bottom;
  sx := X/r.Width;
  result.cx := Round(sx*100.0);
  sy := Y/r.Height;
  result.cy := Round(sy*100.0);
end;

function TMainDialog.CalcMarkRect(out Rect :TRect) :boolean;
var
  iz, ix, iy :integer;
  sz, sx, sy :single;
  r :TRect;
  w, h, x, y :single;
begin
  r := PaintBoxMrkPreview.ClientRect;
  result := TryStrToInt(EditMrkSize.Text, iz)
    and TryStrToInt(EditMrkX.Text, ix)
    and TryStrToInt(EditMrkY.Text, iy);
  if result then begin
    sz := iz/100.0; sx := ix/100.0; sy := iy/100.0;

    w := r.Width*sz;
    h := w/MRKRECTRATIO;
    x := (r.Width-w)*sx;
    y := (r.Height-h)*sy;
    Rect.Left := round(x);
    Rect.Top := round(y);
    Rect.Right := round(x + w);
    Rect.Bottom:= round(y + h);
  end;
end;

procedure TMainDialog.PaintBoxMrkPreviewMouseLeave(Sender: TObject);
begin
  LabelMrkSpace.Caption := '';
end;

function TMainDialog.MouseToMrkSpace(X, Y :integer; out Value :TSize) :boolean;
var
  r :TRect;
  iz :integer;
  sz, sx, sy :single;
begin
  result := TryStrToInt(EditMrkSize.Text, iz);
  if result then begin
    sz := iz/100.0;
    r := PaintBoxMrkPreview.ClientRect;
    sx := X/(r.width*(1-sz));
    sy := Y/(r.Height-r.Width*sz/MRKRECTRATIO);
    Value.cx := round(sx*100.0);
    if Value.cx<0 then Value.cx := 0 else if Value.cx>100 then Value.cx := 100;
    Value.cy := round(sy*100.0);
    if Value.cy<0 then Value.cy := 0 else if Value.cy>100 then Value.cy := 100;
  end;
end;

procedure TMainDialog.PaintBoxMrkPreviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  r :TRect;
begin
  if CalcMarkRect(r) then begin
    FMrkDragging := r.Contains(TPoint.Create(X, Y));
    if FMrkDragging then begin
      FMrkOffset.cx := X - r.Left;
      FMrkOffset.cy := Y - r.Top;
      Exit;
    end;
  end;
  with MouseToSpace(X, Y) do begin
    UpDownMrkX.Position := cx;
    UpDownMrkY.Position := cy;
  end;
end;

procedure TMainDialog.PaintBoxMrkPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  r :TRect;
  MrkSpace :TSize;
begin
  if FMrkDragging then begin
    if MouseToMrkSpace(X - FMrkOffset.cx, Y - FMrkOffset.cy, MrkSpace) then begin
      UpDownMrkX.Position := MrkSpace.cx;
      UpDownMrkY.Position := MrkSpace.cy;
      PaintBoxMrkPreview.Invalidate;
      LabelMrkSpace.Caption := Format('%3d%% %3d%%', [MrkSpace.cx, MrkSpace.cy]);
    end;
  end else begin
    with MouseToSpace(X, Y) do
      LabelMrkSpace.Caption := Format('%3d%% %3d%%', [cx, cy]);
    if CalcMarkRect(r) and r.Contains(TPoint.Create(X, Y)) then
      PaintBoxMrkPreview.Cursor := crHandPoint
    else
      PaintBoxMrkPreview.Cursor := crCross;
  end;
end;

procedure TMainDialog.PaintBoxMrkPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  MrkSpace :TSize;
begin
  if FMrkDragging then begin
    FMrkDragging := false;
    if MouseToMrkSpace(X - FMrkOffset.cx, Y - FMrkOffset.cy, MrkSpace) then begin
      UpDownMrkX.Position := MrkSpace.cx;
      UpDownMrkY.Position := MrkSpace.cy;
    end;
  end else
    PaintBoxMrkPreview.Cursor := crHandPoint;
end;

procedure TMainDialog.PaintBoxMrkPreviewMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);

  procedure ChangePos(Dir :integer);
  var
    v :integer;
  begin
    with UpDownMrkSize do begin
      v := Position + Dir*5;
      if v<0 then v := 0 else if v>100 then v := 100;
      v := (v div 5) * 5;
      if v<1 then v := 1 else if v>100 then v := 100;
      Position := v
    end;
  end;

begin
  if Shift = [ssCtrl] then begin
    ChangePos(Sign(WheelDelta));
    Handled := true;
  end;
end;

procedure TMainDialog.PaintBoxMrkPreviewPaint(Sender: TObject);
var
  op :single;
  cr, r :TRect;
  Img :TBGRABitmap;
  bkg :TBGRAPixel;
  frm :TBGRAPixel;
  mrk :TBGRAPixel;
begin
  cr := PaintBoxMrkPreview.ClientRect;
  bkg.FromColor($00F7DBCC);
  frm.FromColor(clBlack);
  Img := TBGRABitmap.Create(cr.Width, cr.Height, bkg);
  try
    op := StrToFloat(EditMrkAlpha.Text)/100.0;
    mrk.FromColor($00FF8000, round(255*op));
    frm.FromColor(clNavy, round(30+225*op));
    if CalcMarkRect(r) then
      Img.Rectangle(r, frm, mrk, dmDrawWithTransparency);
    Img.Draw(PaintBoxMrkPreview.Canvas, cr);
  except
  end;
  Img.Free;
end;

procedure TMainDialog.EditDstFolderChange(Sender: TObject);
begin
 LabelDstFolderRequired.Enabled := Length(EditDstFolder.Text) = 0;
end;

procedure TMainDialog.EditMrkSizeChange(Sender: TObject);
begin
  PaintBoxMrkPreview.Invalidate;
end;

procedure TMainDialog.EditSizesChange(Sender: TObject);
begin
  LabelSizesRequired.Enabled := Length(EditSizes.Text) = 0;
end;

procedure TMainDialog.ActionSaveAsExecute(Sender: TObject);
begin
  SaveAsDialog.Filename := FIniFilename;
  if SaveAsDialog.Execute then begin
    SaveToFile(SaveAsDialog.Filename);
  end;
end;

procedure TMainDialog.ActionBrowseFilenamesExecute(Sender: TObject);
begin
  if OpenDialogSrcFilenames.Execute then begin
    MemoSrcFilenames.Lines.AddStrings(OpenDialogSrcFilenames.Files);
  end;
end;

procedure TMainDialog.ActionBrowseMrkFilenameExecute(Sender: TObject);
begin
  OpenDialogMrkFilename.Filename := EditMrkFilename.Text;
  if OpenDialogMrkFilename.Execute then begin
    EditMrkFilename.Text := OpenDialogMrkFilename.Filename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
  end;
end;

procedure TMainDialog.ActionBrowseDstFolderExecute(Sender: TObject);
var
  s :string;
  f :string;
begin
  s := EditDstFolder.Text;
  f := '';
  if Pos('%SIZE%', s)>0 then
    f := ExtractFilename(s);
  BrowseDstFolder.Filename := LeftStr(s, Length(s)-Length(f));
  if BrowseDstFolder.Execute then begin
    EditDstFolder.Text := IncludeTrailingPathDelimiter(BrowseDstFolder.FileName)+f;
  end;
end;

procedure TMainDialog.ActionAboutExecute(Sender: TObject);
begin
  TAboutDialog.Execute(IMGRESGUICPR, 'Processor V'+IMGRESVER, LICENSE);
end;

procedure TMainDialog.ActionClearFilenamesExecute(Sender: TObject);
begin
  MemoSrcFilenames.Lines.Clear;
end;

procedure TMainDialog.ActionClearSizesExecute(Sender: TObject);
begin
  with EditSizes do if (SelLength>0) and (SelLength<Length(Text)) then
    SelText := ''
  else
    EditSizes.Text := '';
  UpdateSizes;
end;

procedure TMainDialog.ActionEditWatermarkExecute(Sender: TObject);
var
  MrkFilename :string;
begin
  if TMrkEditDialog.GetFilename(GUIREGKEY, MrkFilename) then begin
    SetMrkSource(msFile);
    EditMrkFilename.Text := MrkFilename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
    PageControl.ActivePage := TabSheetMrk;
  end;
end;

procedure TMainDialog.TimerProgressBarOffTimer(Sender: TObject);
begin
  ProgressBar.Visible := false;
end;

procedure TMainDialog.SetTitle(const Str: string);
begin
  Caption := 'Image Resize - '+Str;
end;

procedure TMainDialog.SetMrkSource(Value: integer);
begin
  if (Value=FMrkSource) or (Value<0) or (Value>2) then Exit;
  case FMrkSource of
  msFile: PanelMrkSourceFile.Visible := false;
  msImage: PanelMrkSourceImage.Visible := false;
  end;
  FMrkSource := Value;
  CheckBoxMrkEnabled.Checked := FMrkSource<>msDisabled;
  case FMrkSource of
  msFile: PanelMrkSourceFile.Visible := true;
  msImage: PanelMrkSourceImage.Visible := true;
  end;
  ButtonMrkEdit.Visible := FMrkSource<>msDisabled;
  GroupBoxMrkLayout.Visible := FMrkSource<>msDisabled;
end;

function TMainDialog.GetMrkSource: integer;
begin
  result := FMrkSource;
end;

procedure TMainDialog.OnPrint(Sender: TObject; const Line: string);
begin
  Log(Line);
  Application.ProcessMessages;
  if FCancelled then
    (Sender as TImgRes).Cancel;
end;

procedure TMainDialog.OnProgress(Sender: TObject; Progress: single);
begin
  FProgress := Progress;
  ProgressBar.Position := round(Progress*100.0);
  Application.ProcessMessages;
  if FCancelled then
    (Sender as TImgRes).Cancel;
end;

procedure TMainDialog.ActionExecuteExecute(Sender: TObject);
var
  Sizes :TIntegerDynArray;
  x :single;
  p :TPos;
  DstFolder :string;
  IntValue :integer;
begin
  if FExecuting then begin
    FCancelled := true;
  end else begin
    ActionExecute.Caption := 'Cancel';
    ButtonExecute.Caption := 'Cancel';
    ActionExecute.ImageIndex := 5;
    ButtonExecute.ImageIndex := 1;
    ButtonExecute.Invalidate;
    ProgressBar.Position := 0;
    ProgressBar.Visible := true;
    TimerProgressBarOff.Enabled := false;
    MemoMessages.Lines.Clear;
    FExecuting := true;
    FCancelled := false;
    Application.ProcessMessages;
    try
      try
        // Source filenames
        if (MemoSrcFilenames.Text='') then
          raise Exception.Create('Missing source filenames.');

        // Destination folder
        if (EditDstFolder.Text='') then
          raise Exception.Create('Missing destination folder.');

        // Sizes
        if EditSizes.Text='default' then begin
          SetLength(Sizes, 1);
          Sizes[0] := 640;
        end else if not TrySizesStrToSizes(EditSizes.Text, Sizes) then
          raise Exception.Create('Invalid Sizes string.');

        // Quality
        FImgRes := TImgRes.Create;
        if not TImgRes.TryStrToJpgQuality(ComboBoxJpgQuality.Text, IntValue) then
          raise Exception.Create('Invalid jpg quality.');
        FImgRes.JpgQuality := IntValue;
        FImgRes.PngCompression := ComboBoxPngCompression.ItemIndex;

        // Rename
        if CheckBoxRenEnabled.Checked then begin
          if RadioButtonRenSimple.Checked then
            FImgRes.DstFiletemplate := RENSIMPLETEMPLATE
          else if RadioButtonRenAdvanced.Checked then
            FImgRes.DstFiletemplate := RENADVANCEDTEMPLATE
          else
            FImgRes.DstFiletemplate := EditRenTemplate.Text;
        end else
          FImgRes.DstFiletemplate := '';

        // Watermark
        if GetMrkSource = msFile then begin
          FImgRes.MrkSource := msFile;
          FImgRes.MrkFilename := EditMrkFilename.Text;

          if not TryStrToFloat(EditMrkSize.Text, x) or (x<0.0) or (x>100.0) then
            raise Exception.Create('Invalid watermark size.');
          FImgRes.MrkSize := x;

          if not TryStrToFloat(EditMrkX.Text, p.x) or (p.x<0.0) or (p.x>100.0) then
            raise Exception.Create('Invalid watermark x border.');
          if not TryStrToFloat(EditMrkY.Text, p.y) or (p.y<0.0) or (p.y>100.0) then
            raise Exception.Create('Invalid watermark y border.');

          FImgRes.MrkX := StrToFloat(EditMrkX.Text);
          FImgRes.MrkY := StrToFloat(EditMrkY.Text);

          if not TryStrToFloat(EditMrkAlpha.Text, x) or (x<0.0) or (x>100.0) then
            raise Exception.Create('Invalid watermark opacity.');
          FImgRes.MrkAlpha := x;
        end else begin
          FImgRes.MrkSource := msDisabled;
          FImgRes.MrkFilename := '';
        end;

        // Hook the processor
        FImgRes.OnPrint := @OnPrint;
        FImgRes.OnProgress := @OnProgress;

        // warn, if %SIZE% placeholder is not containes either in
        // DstFolder nor in FileTemplate
        DstFolder := EditDstFolder.Text;
        if (Length(Sizes)>1) and (Pos('%SIZE%', DstFolder)=0)
         and not (FImgRes.RenEnabled and (Pos('%SIZE%', FImgRes.DstFiletemplate)>0)) then
          raise Exception.Create('Enter playholder %SIZE% to either the destination folder or the file template.');

        FImgRes.Sizes := SizesToSizesStr(Sizes);
        FImgRes.SrcFilenames := MemoSrcFilenames.Lines;
        FImgRes.DstFolder := DstFolder;
        FImgRes.ThreadCount := BoostStrToThreadCount(ComboBoxBoost.Text);
        FImgRes.StopOnError := CheckBoxStopOnError.Checked;
        FImgRes.Execute;

      except on E :Exception do
        begin
          Log(Format('Error at %.0f%%: %s', [FProgress*100.0, E.Message]));
        end;
      end;
    finally
      if FCancelled then Log(Format('Cancelled at %.0f%%', [FProgress*100.0]));
      FImgRes.Free;
      FExecuting := false;
      ActionExecute.Caption := 'E&xecute';
      ButtonExecute.Caption := 'Execute';
      ActionExecute.ImageIndex := 4;
      ButtonExecute.ImageIndex := 2;
      TimerProgressBarOff.Enabled := true;
    end;
  end;

end;

procedure TMainDialog.ActionHelpExecute(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('','HTML/index.html');
//  OpenUrl('http://www.atomek.de/imageresize/index.html#gui');
end;

end.

