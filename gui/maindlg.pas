unit maindlg;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2023 Jan Schirrmacher, www.atomek.de
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
  LCLTranslator, Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ActnList, ExtCtrls, imgres, aboutdlg, inifiles, strutils,
  LMessages, LCLIntf, Buttons, ImgList, LCLType, LazHelpHTML,
  BGRABitmap, BGRABitmapTypes, Generics.Collections, WinDirs,
  mrkeditdlg;

const

  IMGRESGUIVER    = '3.2';
  IMGRESGUICPR    = 'ImageResize V'+IMGRESGUIVER+' © 2023 Jan Schirrmacher, www.atomek.de';

  INITYPE         = 'IRS';
  INIVERSION200   = '200';
  INIVERSION210   = '210';
  INIVERSION      = '300';

  COMMONSECTION   = 'Common';
  SETTINGSSECTION = 'Settings';

  MRKRECTRATIO    = 3.0;

  LINESEP         = '|';

  LM_RUN          = LM_USER + 1;

  RENSIMPLETEMPLATE = 'img%INDEX:1,3%.%FILEEXT%';
  RENADVANCEDTEMPLATE = 'img%INDEX:1,3%_%SIZE%.%FILEEXT%';

  THUMBNAILIMGMAX = 240;
  DOCIMGMAX       = 960;
  DEFAULTSIZE     = 640;

  SIZEBTNHINTFMT  = '%s - %dpx';

resourcestring
  SUrlWebHelp = 'http://www.atomek.de/imageresize/hlp32/gui/en';
  SLocDirHelp = 'hlp\en';
  STxtLicense =
    'ImageResize Copyright (c) 2023 Jan Schirrmacher, www.atomek.de'#10#10+
    'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy and merge copies of the Software, subject to the following conditions:'#10#10+
    'The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.'#10#10+
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR OR COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';

//Image Resize Copyright (c) 2023 Jan Schirrmacher, www.atomek.de
//Hiermit wird jeder Person, die eine Kopie dieser Software und der dazugehörigen Dokumentationsdateien (die „Software“) erhält, kostenlos die Erlaubnis erteilt, uneingeschränkt mit der Software umzugehen, einschließlich, aber nicht beschränkt auf die Rechte zur Nutzung, Vervielfältigung und Zusammenführung von Kopien der Software, vorbehaltlich der folgenden Bedingungen:
//Der obige Urheberrechtshinweis und dieser Genehmigungshinweis müssen in allen Kopien oder wesentlichen Teilen der Software enthalten sein.
//DIE SOFTWARE WIRD OHNE MÄNGELGEWÄHR BEREITGESTELLT, OHNE AUSDRÜCKLICHE ODER STILLSCHWEIGENDE GEWÄHRLEISTUNG, EINSCHLIESSLICH, ABER NICHT BESCHRÄNKT AUF GEWÄHRLEISTUNGEN DER MARKTFÄHIGKEIT, EIGNUNG FÜR EINEN BESTIMMTEN ZWECK UND NICHTVERLETZUNG VON RECHTEN DRITTER. DER AUTOR ODER URHEBERRECHTSINHABER IST IN KEINEM FALL HAFTBAR FÜR ANSPRÜCHE, SCHÄDEN ODER SONSTIGE HAFTUNG, OB AUS VERTRAG, UNERLAUBTER HANDLUNG ODER ANDERWEITIG, DIE SICH AUS, AUS ODER IM ZUSAMMENHANG MIT DER SOFTWARE ODER DER NUTZUNG ODER ANDEREN HANDLUNGEN MIT DER SOFTWARE ERGEBEN.


type

  { TMainDialog }

  TSizeDict = specialize TDictionary<integer, integer>;

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TMainDialog = class(TForm)
    ActionBrowseSrcFolder: TAction;
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
    Bevel1: TBevel;
    BrowseSrcFolder: TSelectDirectoryDialog;
    ButtonBrowseMrkFilename: TBitBtn;
    ButtonBrowseSrcFolder: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonBrowseDstFolder: TBitBtn;
    ButtonClearSrcFiles: TBitBtn;
    ButtonBrowseSrcFiles: TBitBtn;
    ButtonMrkEdit: TBitBtn;
    ButtonExecute: TBitBtn;
    BrowseDstFolder: TSelectDirectoryDialog;
    CheckBoxRenEnabled: TCheckBox;
    CheckBoxMrkEnabled: TCheckBox;
    CheckBoxShake: TCheckBox;
    CheckBoxStopOnError: TCheckBox;
    ComboBoxShakeSeed: TComboBox;
    EditSrcFolder: TEdit;
    EditSrcMasks: TEdit;
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
    GroupBoxMrkFilename: TGroupBox;
    GroupBoxShaking: TGroupBox;
    GroupBoxDstFolder: TGroupBox;
    GroupBoxParams: TGroupBox;
    GroupBoxImageFiles: TGroupBox;
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
    LabelShakeSeed: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label4: TLabel;
    LabelSrcFilenamesRequired: TLabel;
    LabelSourceFileListMessage: TLabel;
    Label9: TLabel;
    LabelCores: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    LabelMrkSpace: TLabel;
    LabelSizesRequired: TLabel;
    LabelDstFolderRequired: TLabel;
    Label2: TLabel;
    MemoMessages: TMemo;
    MemoSrcFilenames: TMemo;
    NotebookFileSource: TNotebook;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PageFilelist: TPage;
    PagePathMask: TPage;
    PageControlParams: TPageControl;
    PaintBoxMrkPreview: TPaintBox;
    PanelControls: TPanel;
    PanelPreview: TPanel;
    ProgressBar: TProgressBar;
    RadioButtonFilelist: TRadioButton;
    RadioButtonPathMask: TRadioButton;
    RadioButtonRenSimple: TRadioButton;
    RadioButtonRenCustom: TRadioButton;
    RadioButtonRenAdvanced: TRadioButton;
    SaveAsDialog: TSaveDialog;
    Splitter1: TSplitter;
    TabSheetRenaming: TTabSheet;
    TabSheetMrk: TTabSheet;
    TabSheetRessources: TTabSheet;
    TabSheetSizes: TTabSheet;
    TabSheetQuality: TTabSheet;
    TimerProgressBarOff: TTimer;
    ToolBar: TToolBar;
    ToolBarSizeButtons: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButtonAbout: TToolButton;
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
    procedure CheckBoxMrkEnabledChange(Sender: TObject);
    procedure EditSrcFolderChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionBrowseSrcFolderExecute(Sender: TObject);
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
    procedure CheckBoxRenEnabledClick(Sender: TObject);
    procedure EditRenTemplateEnter(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure EditDstFolderChange(Sender: TObject);
    procedure EditMrkSizeChange(Sender: TObject);
    procedure EditSizesChange(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
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
    procedure PanelControlsResize(Sender: TObject);
    procedure RadioButtonFilelistChange(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
//    procedure CheckBoxMrkEnabledChange(Sender: TObject);
    procedure EditSizesExit(Sender: TObject);
  private
    FAutoExit :boolean;
    FIsSave :boolean;
    FIniFilename :string;
    FImgRes :TImgRes;
    FCancelled :boolean;
    FExecuting :boolean;
    FProgress :single;
    FMrkDragging :boolean;
    FMrkOffset :TSize; // while dragging
    function GetSettingsFilename(CanCreate :boolean) :string;
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnProgress(Sender :TObject; Progress :single);
    function LoadFromIni(Ini :TCustomIniFile) :boolean;
    procedure SaveToIni(Ini :TCustomIniFile);
    function LoadSettings :boolean;
    procedure SaveSettings;
    function LoadFromFile(const Filename :string) :boolean;
    procedure SaveToFile(const Filename :string);
    procedure Log(const Msg :string);
    function BoostStrToThreadCount(const Value :string) :integer;
    function ThreadCountToBoostStr(ThreadCount :integer) :string;
    procedure UpdateSizesControls;
    procedure UpdateRequireLabels;
    procedure UpdateSrcFilesRequireLabel;
    procedure UpdateDstFolderRequireLabel;
    procedure UpdateSizesRequireLabel;
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
  math, helpintfs, Windows, FileUtil;

const
  SCptRandom = '<random>';
  SCptSizesDefault = 'default';
  SCptSingleThread = 'single';
  SCptMaximumThread = 'maximum';

resourcestring
  SCptUnnamed = '<unnamed>';
  SErrInvalidShakeSeed = 'Invalid Shake Seed.';
  SCptTitlePrefix = 'Image Resize - ';
  SCptLastSettings = '<last settings>';
  SCptCancel = 'Cancel';
  SCptExecute = 'Execute';
  SCptExecuteA = 'E&xecute';
  SCptSizeClassSmall = 'Small - Thumbnail size';
  SCptSizeClassMedium = 'Medium - Document size';
  SCptSizeClassLarge = 'Large - Desktop size';
  SCptCoresFmt = '%d Cores';
  SCptInfoFmt = 'ImageResize %s, Processor %s, %d Cores';
  SLogErrorPrefix = 'Error - ';
  SLogCantLoad = 'Cant load Image Resize Settings File';
  SLogWarningVersionFmt = 'Warning: Unexpected format %s (%s expected).';
  SCptFileNotFoundFmt = 'File ''%s'' not found.';
  SLogFileSavedToFmt = 'Settings saved to ''%s''';
  SCptSourceFilesFmt = '%d source files';
  SErrMissingSourceFilenames = 'Missing source filenames.';
  SErrMissingSourceFolder = 'Missing source folder.';
  SErrMissingDestinationFolder = 'Missing destination folder.';
  SErrInvalidSizes = 'Invalid Sizes string.';
  SErrInvalidJpgQuality = 'Invalid jpg quality.';
  SErrInvalidMrkSize = 'Invalid watermark size.';
  SErrInvalidMrkXBrd = 'Invalid watermark x border.';
  SErrInvalidMrkYBrd = 'Invalid watermark y border.';
  SErrInvalidMrkOpacity = 'Invalid watermark opacity.';
  SErrEnterPlaceholder = 'Enter placeholder %SIZE% to either the destination folder or the file template.';
  SErrAtFmt = 'Error at %.0f%% - %s';
  SErrCancelledAtFmt = 'Cancelled at %.0f%%';

{$R *.lfm}

procedure SetRequiredState(Control :TLabel; Value :boolean);
begin
  if Value then begin
    Control.Font.Color := clRed;
    Control.Caption := 'Ø';
  end else begin
    Control.Font.Color := clNavy;
    Control.Caption := 'ü';
  end;
end;

function StrToShakeSeed(const Value :string) :integer;
begin
  if Value=SCptRandom then
    result := 0
  else
    if not TryStrToInt(Value, result) then raise Exception.Create(SErrInvalidShakeSeed);
end;

function ShakeSeedToStr(Value :integer) :string;
begin
  if Value<=0 then
    result := SCptRandom
  else
    result := IntToStr(Value);
end;

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
  AllowDropFiles := true;

  if (SysLocale.PriLangId=7) and not Application.HasOption('E', 'EN') then
    SetDefaultLang('de');

  // Create Size Buttons
  for i:=0 to High(DEFSIZES) do begin
    Button := TToolButton.Create(ToolBarSizeButtons);
    Button.Parent := ToolBarSizeButtons;
    Button.Caption := IntToStr(DEFSIZES[i]);
    if DEFSIZES[i]<=THUMBNAILIMGMAX then
      Cpt := SCptSizeClassSmall
    else if DEFSIZES[i]<=DOCIMGMAX then
      Cpt := SCptSizeClassMedium
    else
      Cpt := SCptSizeClassLarge;

    Button.Hint := Format(SIZEBTNHINTFMT, [Cpt, DEFSIZES[i]]);
    if DEFSIZES[i]<=THUMBNAILIMGMAX then
        Button.ImageIndex := 5
    else if DEFSIZES[i]<=DOCIMGMAX then
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
  LocHelpDir :string;
begin

  if not LoadSettings then
    ActionNew.Execute;
  UpdateRequireLabels;
  FAutoExit := Application.HasOption('X', 'AUTOEXIT');
  Params := Application.GetNonOptions('AXE', ['AUTOSTART', 'AUTOEXIT', 'EN']);
  if Length(Params)=1 then
    LoadFromFile(Params[0]);
  if Application.HasOption('A', 'AUTOSTART') then
    PostMessage(Handle, LM_RUN, 0, 0);

  PageControlParams.ActivePageIndex := 0;

  // Show number of cores
  LabelCores.Caption := Format(SCptCoresFmt, [TThread.ProcessorCount]);

  // Show initial message in Message log
  Log(Format(SCptInfoFmt, [IMGRESGUIVER, IMGRESVER, TThread.ProcessorCount]));

  // If no local help files, then use online help
  LocHelpDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+SLocDirHelp;
  if not DirectoryExists(LocHelpDir) then
    HTMLHelpDatabase.BaseURL := SUrlWebHelp
  else
    HTMLHelpDatabase.BaseURL := 'file://'+LocHelpDir;
  //with SysLocale do
  //  MemoMessages.Lines.Add(Format('DefaultLCID=%d PriLangID=%d SubLangID=%d', [DefaultLCID, PriLangID, SubLangID]));
end;

procedure TMainDialog.LMRun(var Message: TLMessage);
begin
  ActionExecute.Execute;
  Application.ProcessMessages;
  if FAutoExit then begin
    Close;
  end;

end;

procedure TMainDialog.EditSrcFolderChange(Sender: TObject);
begin
   UpdateSrcFilesRequireLabel;
end;

procedure TMainDialog.CheckBoxMrkEnabledChange(Sender: TObject);
var
  Vis :boolean;
begin
  Vis := CheckBoxMrkEnabled.Checked;
  GroupBoxMrkFilename.Visible := Vis;
  GroupBoxMrkLayout.Visible := Vis;
end;

procedure TMainDialog.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log(SLogErrorPrefix+E.Message);
end;

procedure TMainDialog.ActionBrowseSrcFolderExecute(Sender: TObject);
begin
  BrowseSrcFolder.Filename := EditDstFolder.Text;
  if BrowseSrcFolder.Execute then begin
    EditSrcFolder.Text := IncludeTrailingPathDelimiter(BrowseSrcFolder.FileName);
  end;

end;

procedure TMainDialog.CheckBoxRenEnabledClick(Sender: TObject);
var
  Vis :boolean;
begin
  Vis := CheckBoxRenEnabled.Checked;
  GroupBoxRename.Visible := Vis;
  GroupBoxShaking.Visible := Vis;
end;

procedure TMainDialog.EditRenTemplateEnter(Sender: TObject);
begin
  RadioButtonRenCustom.Checked := true;
end;

procedure TMainDialog.EditSizesExit(Sender: TObject);
begin
  UpdateSizesControls;
end;

function TMainDialog.GetSettingsFilename(CanCreate :boolean): string;
begin
  result := IncludeTrailingPathDelimiter(GetWindowsSpecialDir(FOLDERID_LocalAppData, CanCreate))+ChangeFileExt(ExtractFileName(Application.ExeName), '.'+IMGRESGUIVER)+'\settings.ini';
end;

procedure TMainDialog.ActionNewExecute(Sender: TObject);
var
  ImgResizer :TImgRes;
begin
  MemoMessages.Lines.Clear;
  ImgResizer := TImgRes.Create;
  try
    MemoSrcFilenames.Lines.Clear;
    EditSrcFolder.Text := '';
    EditSrcMasks.Text := '*.jpg; *.png';
    EditDstFolder.Text := '';
    EditSizes.Text := '';
    RadioButtonFileList.Checked := true;
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
    CheckBoxShake.Checked := DEFAULT_SHAKE;
    ComboBoxShakeSeed.Text := SCptRandom;
    CheckBoxMrkEnabled.Checked := false;
    UpdateSizesControls;
    UpdateRequireLabels;
    PageControlParams.ActivePage := TabSheetSizes;
    SetTitle(SCptUnnamed);
  finally
    ImgResizer.Free;
  end;
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
       Log(SLogCantLoad);
       Exit;
    end;
    IniVer := Ini.ReadString('Common', 'Version', '000');
    result := (IniVer=INIVERSION) or (IniVer=INIVERSION200) or (IniVer=INIVERSION210);
    if not result then begin
      Log(Format(SLogWarningVersionFmt, [IniVer, INIVERSION]));
      Exit;
    end;
    case ReadInteger(SETTINGSSECTION, 'SrcMode', NotebookFileSource.PageIndex) of
    0: RadioButtonFileList.Checked := true;
    1: RadioButtonPathMask.Checked := true;
    end;
    EditSrcFolder.Text := ReadString(SETTINGSSECTION, 'SrcFolder', EditSrcFolder.Text);
    EditSrcMasks.Text := ReadString(SETTINGSSECTION, 'SrcMasks', EditSrcMasks.Text);
    MemoSrcFilenames.Text := ReplaceStr(ReadString(SETTINGSSECTION, 'SrcFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP)), LINESEP, #13#10);
    EditDstFolder.Text := ReadString(SETTINGSSECTION, 'DstFolder', EditDstFolder.Text);
    EditSizes.Text := ReadString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    UpdateSizesControls;
    ComboBoxJpgQuality.Text := ReadString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    ComboBoxPngCompression.Text := ReadString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    CheckBoxMrkEnabled.Checked := ReadBool(SETTINGSSECTION, 'MrkEnabled', CheckBoxMrkEnabled.Checked);
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
    CheckBoxShake.Checked := ReadBool(SETTINGSSECTION, 'Shake', CheckBoxShake.Checked);
    ComboBoxShakeSeed.Text := ShakeSeedToStr(ReadInteger(SETTINGSSECTION, 'ShakeSeed', 0));
  end;
end;

procedure TMainDialog.SaveToIni(Ini :TCustomIniFile);
begin
  // Navigate to proper "directory":
  with Ini do begin
    WriteString(COMMONSECTION, 'Type', INITYPE);
    WriteString(COMMONSECTION, 'Version', INIVERSION);
    EraseSection(SETTINGSSECTION);
    WriteInteger(SETTINGSSECTION, 'SrcMode', NotebookFileSource.PageIndex);
    WriteString(SETTINGSSECTION, 'SrcFolder', EditSrcFolder.Text);
    WriteString(SETTINGSSECTION, 'SrcMasks', EditSrcMasks.Text);
    WriteString(SETTINGSSECTION, 'DstFolder', EditDstFolder.Text);
    WriteString(SETTINGSSECTION, 'SrcFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP));
    WriteString(SETTINGSSECTION, 'DstFolder', EditDstFolder.Text);
    WriteString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    WriteString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    WriteString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    WriteBool(SETTINGSSECTION, 'MrkEnabled', CheckBoxMrkEnabled.Checked);
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
    WriteBool(SETTINGSSECTION, 'Shake', CheckBoxShake.Checked);
    WriteInteger(SETTINGSSECTION, 'ShakeSeed', StrToShakeSeed(ComboBoxShakeSeed.Text));
  end;
end;

function TMainDialog.LoadSettings: boolean;
var
  Ini :TIniFile;
  Filename :string;
begin
  Filename := GetSettingsFilename(false);
  if not FileExists(Filename) then Exit(false);
  Ini := TIniFile.Create(Filename);
  try
    result := LoadFromIni(Ini);
  finally
    Ini.Free;
  end;
  if result then
    SetTitle(SCptLastSettings);
end;

procedure TMainDialog.SaveSettings;
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create(GetSettingsFilename(true));
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
    raise Exception.CreateFmt(SCptFileNotFoundFmt, [Filename]);
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
    Log(Format(SLogFileSavedToFmt, [FIniFilename]));
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
  UpdateSizesControls;
  UpdateRequireLabels;
end;

procedure TMainDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;
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
  if s=SCptSingleThread then
    result := 1
  else if s=SCptMaximumThread then
    result := 0
  else
    result := StrToInt(s);
end;

function TMainDialog.ThreadCountToBoostStr(ThreadCount: integer): string;
begin
  case ThreadCount of
  0:
    result := SCptMaximumThread;
  1:
    result := SCptSingleThread;
  else
    result := IntToStr(ThreadCount);
  end;
end;

procedure TMainDialog.UpdateSizesControls;
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

procedure TMainDialog.UpdateRequireLabels;
begin
  UpdateSrcFilesRequireLabel;
  UpdateDstFolderRequireLabel;
  UpdateSizesRequireLabel;
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
  UpdateSrcFilesRequireLabel;
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

procedure TMainDialog.UpdateSrcFilesRequireLabel;
begin
  SetRequiredState(LabelSrcFilenamesRequired, RadioButtonFilelist.Checked and (Length(MemoSrcFilenames.Text) = 0) or RadioButtonPathMask.Checked and (Length(EditSrcFolder.text) = 0));
  LabelSourceFileListMessage.Caption := Format(SCptSourceFilesFmt, [MemoSrcFilenames.Lines.Count]);
end;

procedure TMainDialog.UpdateDstFolderRequireLabel;
begin
  SetRequiredState(LabelDstFolderRequired, Length(EditDstFolder.Text) = 0);
end;

procedure TMainDialog.UpdateSizesRequireLabel;
begin
  SetRequiredState(LabelSizesRequired, Length(EditSizes.Text) = 0);
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
    if sz>=1.0 then
      sx := 0
    else
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

procedure TMainDialog.PanelControlsResize(Sender: TObject);

  procedure Place(Control :TControl; Target :TControl);
  var
    GroupBox :TGroupBox;
  begin
    Control.Left := Target.Left +8;
    Control.Top := Target.Top;
    if Target is TGroupBox then begin
      GroupBox := TGroupBox(Target);
      if GroupBox.Caption[1] <> ' ' then
        GroupBox.Caption := '      '+GroupBox.Caption;
    end;
  end;

begin
  Place(LabelSrcFilenamesRequired, GroupBoxImageFiles);
  Place(LabelDstFolderRequired, GroupBoxDstFolder);
  Place(LabelSizesRequired, GroupBoxParams);
end;

procedure TMainDialog.RadioButtonFilelistChange(Sender: TObject);
begin
  NotebookFileSource.PageIndex := (Sender as TRadioButton).Tag;
  UpdateSrcFilesRequireLabel;
end;

procedure TMainDialog.EditDstFolderChange(Sender: TObject);
begin
  UpdateDstFolderRequireLabel;
end;

procedure TMainDialog.EditMrkSizeChange(Sender: TObject);
begin
  PaintBoxMrkPreview.Invalidate;
end;

procedure TMainDialog.EditSizesChange(Sender: TObject);
begin
  UpdateSizesRequireLabel;
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
  TAboutDialog.Execute(IMGRESGUICPR, 'Processor V'+IMGRESVER, STxtLicense);
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
  UpdateSizesControls;
end;

procedure TMainDialog.ActionEditWatermarkExecute(Sender: TObject);
var
  MrkFilename :string;
begin
  if TMrkEditDialog.GetFilename(MrkFilename) then begin
    EditMrkFilename.Text := MrkFilename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
    PageControlParams.ActivePage := TabSheetMrk;
  end;
end;

procedure TMainDialog.TimerProgressBarOffTimer(Sender: TObject);
begin
  ProgressBar.Visible := false;
end;

procedure TMainDialog.SetTitle(const Str: string);
begin
  Caption := SCptTitlePrefix+Str;
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
  SrcFilenames :TStringList;
begin
  if FExecuting then begin
    FCancelled := true;
  end else begin
    FProgress := 0;
    ActionExecute.Caption := SCptCancel;
    ButtonExecute.Caption := SCptCancel;
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
      SrcFilenames := TStringList.Create;
      try
        // Source filenames
        case NotebookFileSource.PageIndex of
        0:
          begin
            if (MemoSrcFilenames.Text='') then
              raise Exception.Create(SErrMissingSourceFilenames);

            SrcFilenames.Assign(MemoSrcFilenames.Lines);
          end;
        1:
          begin
            if (EditSrcFolder.Text='') or not DirectoryExists(EditSrcFolder.Text) then
              raise Exception.Create(SErrMissingSourceFolder);
            FindAllFiles(SrcFilenames, EditSrcFolder.Text, EditSrcMasks.Text, false);
          end;
        end;

        // Destination folder
        if (EditDstFolder.Text='') then
          raise Exception.Create(SErrMissingDestinationFolder);

        // Sizes
        Sizes := nil;
        if EditSizes.Text = SCptSizesDefault then begin
          SetLength(Sizes, 1);
          Sizes[0] := DEFAULTSIZE;
        end else if not TrySizesStrToSizes(EditSizes.Text, Sizes) then
          raise Exception.Create(SErrInvalidSizes);

        // Quality
        FImgRes := TImgRes.Create;
        if not TImgRes.TryStrToJpgQuality(ComboBoxJpgQuality.Text, IntValue) then
          raise Exception.Create(SErrInvalidJpgQuality);
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
        FImgRes.Shake := CheckBoxShake.Checked;
        FImgRes.ShakeSeed := StrToShakeSeed(ComboBoxShakeSeed.Text);

        // Watermark
        FImgRes.MrkFilename := EditMrkFilename.Text;

        if not TryStrToFloat(EditMrkSize.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkSize);
        FImgRes.MrkSize := x;

        if not TryStrToFloat(EditMrkX.Text, p.x) or (p.x<0.0) or (p.x>100.0) then
          raise Exception.Create(SErrInvalidMrkXBrd);
        if not TryStrToFloat(EditMrkY.Text, p.y) or (p.y<0.0) or (p.y>100.0) then
          raise Exception.Create(SErrInvalidMrkYBrd);

        FImgRes.MrkX := StrToFloat(EditMrkX.Text);
        FImgRes.MrkY := StrToFloat(EditMrkY.Text);

        if not TryStrToFloat(EditMrkAlpha.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkOpacity);
        FImgRes.MrkAlpha := x;

        // Hook the processor
        FImgRes.OnPrint := @OnPrint;
        FImgRes.OnProgress := @OnProgress;

        // warn, if %SIZE% placeholder is not containes either in
        // DstFolder nor in FileTemplate
        DstFolder := EditDstFolder.Text;
        if (Length(Sizes)>1) and (Pos('%SIZE%', DstFolder)=0)
         and not (FImgRes.RenEnabled and (Pos('%SIZE%', FImgRes.DstFiletemplate)>0)) then
          raise Exception.Create(SErrEnterPlaceholder);

        FImgRes.Sizes := SizesToSizesStr(Sizes);
        FImgRes.SrcFilenames := SrcFilenames;
        FImgRes.DstFolder := DstFolder;
        FImgRes.ThreadCount := BoostStrToThreadCount(ComboBoxBoost.Text);
        FImgRes.StopOnError := CheckBoxStopOnError.Checked;
        FImgRes.Execute;

      except on E :Exception do
        begin
          Log(Format(SErrAtFmt, [FProgress*100.0, E.Message]));
        end;
      end;
    finally
      SrcFilenames.Free;
      FImgRes.Free;
      FExecuting := false;
      if FCancelled then Log(Format(SErrCancelledAtFmt, [FProgress*100.0]));
      ActionExecute.Caption := SCptExecuteA;
      ButtonExecute.Caption := SCptExecute;
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

