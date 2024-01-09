unit maindlg;

////////////////////////////////////////////////////////////////////////////////
//
//  ImageResize (c) 2024 Jan Schirrmacher, www.atomek.de
//
//  See https://github.com/Atomek61/ImageResize.git for licensing
//
//  maindlg.pas is the GUI interface for the TProcessor-Processor.
//
////////////////////////////////////////////////////////////////////////////////

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  LCLTranslator, Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ActnList, ExtCtrls, imgres, aboutdlg, inifiles, strutils,
  LMessages, LCLIntf, Buttons, ImgList, LCLType, LazHelpHTML, BGRABitmap,
  BGRABitmapTypes, BGRASpeedButton, RichMemo, Generics.Collections,
  mrkeditdlg, WinDirs, updateutils, settings, logging, loggingrichmemo,
  StringArrays;

const
  GUIVER_APP      = 'ImageResize';
  GUIVER_VERSION  = '3.6';
  GUIVER_DATE     = '2024-01-01';

  GUIVER          :TVersionManifest = (App: GUIVER_APP; Version: GUIVER_VERSION; Date: GUIVER_DATE; Hint: '');

  APPWEBURL       = 'www.atomek.de/imageresize/index.html';
  APPDOWNLOADURL  = 'www.atomek.de/imageresize/download/InstallImageResize.zip';
  APPGITHUBURL    = 'https://github.com/Atomek61/ImageResize';
  GUIVERURL       = 'https://www.atomek.de/imageresize/download/version.manifest';

  IMGRESGUICPR    = 'ImageResize ' + GUIVER_VERSION + ' © 2024 Jan Schirrmacher, www.atomek.de';

  PRJTYPE         = 'IRS';
  PRJVERSION200   = '200';
  PRJVERSION210   = '210';
  PRJVERSION      = '300';

  SETTYPE         = 'IST';
  SETVERSION      = '100';

  LASTPROJECT_FILENAME = 'lastproject.irs';
  SETTINGS_FILENAME = 'settings.ini';
  COMMONSECTION   = 'Common';
  SETTINGSSECTION = 'Settings';
  DIALOGSECTION   = 'Dialog';

  MRKRECTRATIO    = 3.0;

  LINESEP         = '|';

  LM_RUN          = LM_USER + 1;

  RENSIMPLETEMPLATE   = 'img%INDEX:1,3%.%FILEEXT%';
  RENADVANCEDTEMPLATE = 'img%INDEX:1,3%_%SIZE%.%FILEEXT%';

  THUMBNAILIMGMAX = 240;
  DOCIMGMAX       = 960;
  DEFAULTSIZE     = 640;

  SIZEBTNHINTFMT  = '%s - %dpx';

  IMAGEINDEX_START        = 5;
  IMAGEINDEX_CANCEL       = 6;
  IMAGEINDEX_DIRTY        = 4;
  IMAGEINDEX_SAVE         = 2;
  IMAGEINDEX_IMGTHUMBNAIL = 9;
  IMAGEINDEX_IMGDOCUMENT  = 10;
  IMAGEINDEX_IMGSCREEN    = 11;
  IMAGEINDEX_REQUIRED     = 22;
  IMAGEINDEX_NOTREQUIRED  = 23;

  clOrange = $3d69a6;
  clDarkGray = $403040;

  STYLECOLOR_LIGHT  = $00F1DDC9;
  STYLECOLOR_LIGHT2 = $00E6C09B;
  STYLECOLOR_DARK   = $00D59453;

  LOGCOLORS :array[llHint..llCrash] of TColor = (clGray, clDarkGray, clGreen, clOrange, clMaroon, clRed);

resourcestring
  SCptDependenciesFmt = 'Build with Lazarus %s, BGRABitmap %s, dExif %s';
  SUrlWebHelp = 'http://www.atomek.de/imageresize/hlp35/gui/en';
  SLocDirHelp = 'hlp\gui\en';
  STxtLicense =
'ImageResize Copyright (c) 2024 Jan Schirrmacher, www.atomek.de'#10#10+
'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy and merge copies of the Software, subject to the following conditions:'#10#10+
'The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.'#10#10+
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHOR OR COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';

//Image Resize Copyright (c) 2024 Jan Schirrmacher, www.atomek.de
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
    ActionPresentation: TAction;
    ActionListParams: TActionList;
    ActionListSource: TActionList;
    ActionSettings: TAction;
    ActionSrcFolder: TAction;
    ActionSrcFilenames: TAction;
    ActionParamTagging: TAction;
    ActionParamWatermark: TAction;
    ActionParamRenaming: TAction;
    ActionParamSizes: TAction;
    ActionParamQuality: TAction;
    ActionEditWatermark: TAction;
    ActionHelp: TAction;
    ActionSave: TAction;
    ActionSaveAs: TAction;
    ActionOpen: TAction;
    ActionNew: TAction;
    ActionAbout: TAction;
    ActionExecute: TAction;
    ButtonExecute: TBGRASpeedButton;
    CheckBoxNoCreate: TCheckBox;
    CheckBoxImageInfosEnabled: TCheckBox;
    LabelStep1: TLabel;
    LabelStep2: TLabel;
    LabelStep3: TLabel;
    MainActionList: TActionList;
    ApplicationProperties1: TApplicationProperties;
    BrowseSrcFolder: TSelectDirectoryDialog;
    ButtonBrowseMrkFilename: TBitBtn;
    ButtonBrowseSrcFolder: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonBrowseTargetFolder: TBitBtn;
    ButtonClearSrcFiles: TBitBtn;
    ButtonBrowseSrcFiles: TBitBtn;
    ButtonClearCopyright: TBitBtn;
    ButtonInsertSIZE: TBitBtn;
    ButtonInsertCopyright: TBitBtn;
    ButtonMrkEdit: TBitBtn;
    BrowseTargetFolder: TSelectDirectoryDialog;
    CheckBoxTagsSourceTagsFiles: TCheckBox;
    CheckBoxTagsReportEnabled: TCheckBox;
    CheckBoxTagsSourceEXIF: TCheckBox;
    CheckBoxTagTimestamp: TCheckBox;
    CheckBoxTagCopyright: TCheckBox;
    CheckBoxTagTitle: TCheckBox;
    CheckBoxRenEnabled: TCheckBox;
    CheckBoxMrkEnabled: TCheckBox;
    CheckBoxShuffle: TCheckBox;
    ComboBoxResampling: TComboBox;
    ComboBoxShuffleSeed: TComboBox;
    EditCopyright: TEdit;
    EditSrcFolder: TEdit;
    EditSrcMasks: TEdit;
    EditRenTemplate: TComboBox;
    ComboBoxJpgQuality: TComboBox;
    ComboBoxPngCompression: TComboBox;
    EditMrkAlpha: TEdit;
    EditMrkFilename: TEdit;
    EditMrkSize: TEdit;
    EditMrkX: TEdit;
    EditMrkY: TEdit;
    EditSizes: TEdit;
    EditTargetFolder: TEdit;
    GroupBoxResampling: TGroupBox;
    GroupBoxTagsReport: TGroupBox;
    GroupBoxTagsSource: TGroupBox;
    GroupBoxEXIFTagging: TGroupBox;
    GroupBoxMrkFilename: TGroupBox;
    GroupBoxShuffle: TGroupBox;
    GroupBoxRename: TGroupBox;
    GroupBoxMrkLayout: TGroupBox;
    GroupBoxJpgOptions: TGroupBox;
    GroupBoxPngOptions: TGroupBox;
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
    ImageList32x32: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label21: TLabel;
    Label7: TLabel;
    LabelHintPlaceholder1: TLabel;
    LabelShuffleSeed: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    LabelHintPlaceholderTitle: TLabel;
    LabelHintPlaceholder2: TLabel;
    Label4: TLabel;
    LabelSourceFileListMessage: TLabel;
    Label9: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMrkSpace: TLabel;
    MemoSrcFilenames: TMemo;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PageControlSource: TPageControl;
    PageControlParams: TPageControl;
    ProgressBar: TPaintBox;
    PaintBoxStep1: TPaintBox;
    PaintBoxMrkPreview: TPaintBox;
    PaintBoxStep2: TPaintBox;
    PaintBoxStep3: TPaintBox;
    PanelDestination: TPanel;
    PanelParams: TPanel;
    PanelSource: TPanel;
    PanelControls: TPanel;
    PanelPreview: TPanel;
    RadioButtonRenSimple: TRadioButton;
    RadioButtonRenCustom: TRadioButton;
    RadioButtonRenAdvanced: TRadioButton;
    MemoMessages: TRichMemo;
    SaveAsDialog: TSaveDialog;
    SaveAsDialogTagsReport: TSaveDialog;
    TabSheetSrcFilenames: TTabSheet;
    TabSheetSrcFolder: TTabSheet;
    TabSheetTagging: TTabSheet;
    TabSheetRenaming: TTabSheet;
    TabSheetWatermark: TTabSheet;
    TabSheetSizes: TTabSheet;
    TabSheetQuality: TTabSheet;
    TimerProgressBarOff: TTimer;
    ToolBar: TToolBar;
    ToolBarSrc: TToolBar;
    ToolBarParameters: TToolBar;
    ToolBarSizeButtons: TToolBar;
    ToolButton1: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonSlideshowAfterburner: TToolButton;
    ToolButtonSrcFilenames: TToolButton;
    ToolButtonSrcFolder: TToolButton;
    ToolButtonSettings: TToolButton;
    ToolButtonTagging: TToolButton;
    ToolButtonWatermark: TToolButton;
    ToolButtonRenaming: TToolButton;
    ToolButtonQuality: TToolButton;
    ToolButtonSizes: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSaveAs: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButton5: TToolButton;
    ToolButtonExecute: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolButton8: TToolButton;
    ToolButtonEditor: TToolButton;
    UpDownMrkAlpha: TUpDown;
    UpDownMrkSize: TUpDown;
    UpDownMrkX: TUpDown;
    UpDownMrkY: TUpDown;
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionPresentationExecute(Sender: TObject);
    procedure ActionSourceExecute(Sender: TObject);
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonBrowseMrkFilenameClick(Sender: TObject);
    procedure ButtonBrowseSrcFilesClick(Sender: TObject);
    procedure ButtonBrowseSrcFolderClick(Sender: TObject);
    procedure ButtonClearCopyrightClick(Sender: TObject);
    procedure ButtonClearSizesClick(Sender: TObject);
    procedure ButtonClearSrcFilesClick(Sender: TObject);
    procedure ButtonInsertCopyrightClick(Sender: TObject);
    procedure ButtonInsertSIZEClick(Sender: TObject);
    procedure ActionParamExecute(Sender :TObject);
    procedure EditSizesKeyPress(Sender: TObject; var Key: char);
    procedure EditSrcFolderChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionEditWatermarkExecute(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure EditRenTemplateEnter(Sender: TObject);
    procedure EditTargetFolderChange(Sender: TObject);
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
    procedure ProgressBarPaint(Sender: TObject);
    procedure PaintBoxStep1Paint(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
    procedure EditSizesExit(Sender: TObject);
    procedure ProjectChanged(Sender :TObject);
  private
    FProjectFilename :string;
    FProjectDescription :string; // From project file
    FAutoExit :boolean;
    FIsSave :boolean; // If Current project is from a file: if false then save -> saveas
    FIsDirty :boolean; // If Changes since last modification
    FCancelled :boolean;
    FExecuting :boolean;
    FProgress :single;
    FMrkDragging :boolean;
    FMrkOffset :TSize; // while dragging
    FRequiredSteps :array[1..3] of boolean;
    FProcessingSettings :TProcessingSettings;
    FDialogSettings :TDialogSettings;
    FWorkingDirectory :string;
    procedure ChangeCurrentDir(const Path :string);
    function GetAppDataFilename(const Filetitle :string; CanCreate :boolean) :string;
    procedure SetDirty(AValue: boolean);
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string; Level :TLogLevel);
    procedure OnProgress(Sender :TObject; Progress :single);
    function LoadSettings :boolean;
    procedure SaveSettings;
    function LoadProjectFromIni(Ini :TCustomIniFile) :boolean;
    procedure SaveProjectToIni(Ini :TCustomIniFile);
    function LoadLastProject :boolean;
    procedure SaveLastProject;
    function LoadProjectFromFile(const Filename :string) :boolean;
    procedure SaveProjectToFile(const Filename :string);
    function CheckSave :boolean;
//    procedure Log(const Msg :string; Level :TLogLevel = llInfo);
    function BoostStrToThreadCount(const Value :string) :integer;
    function ThreadCountToBoostStr(ThreadCount :integer) :string;
    function GetRequiredSteps(Index : integer) :boolean;
    procedure SetRequiredSteps(Index : integer; AValue: boolean);
    procedure DoRequiredStepsChanged(Index :integer);
    procedure RequiredStepsUpdate;
    procedure UpdateRequiredStep(Index :integer);
    procedure LMRun(var Message: TLMessage); message LM_RUN;
    procedure SizeButtonClick(Sender :TObject);
    function MouseToSpace(X, Y :integer) :TSize;
    function MouseToMrkSpace(X, Y :integer; out Value :TSize) :boolean;
    function CalcMarkRect(out Rect :TRect) :boolean;
    function HasFocus(Control :TWinControl) :boolean;
  public
    property RequiredSteps[Index :integer] :boolean read GetRequiredSteps write SetRequiredSteps;
    property Dirty :boolean read FIsDirty write SetDirty;
  end;

var
  MainDialog: TMainDialog;

implementation

uses
  math, helpintfs, Windows, FileUtil, tags, presentationdlg, settingsdlg;

const
  SCptRandom        = '<random>';
  SCptSizesDefault  = 'default';
  SCptSingleThread  = 'single';
  SCptMaximumThread = 'maximum';

resourcestring
  SCptProcessor                 = 'Processor';
  SMsgQuerySave                 = 'The project has been modified.'+#10+#10+'Do you want to save the changes?';
  SCptQuerySave                 = 'Project unsaved';
  SMsgProjectDescriptionFmt     = 'Description: %s';
  SCptUnnamed                   = '<unnamed>';
  SErrInvalidShuffleSeed        = 'Invalid Shuffle Seed';
  SCptTitlePrefix               = 'ImageResize - ';
  SCptLastProject               = '<last project>';
  SCptCancel                    = 'Cancel';
  SCptExecute                   = 'Execute';
  SCptExecuteA                  = 'E&xecute';
  SCptSizeClassSmall            = 'Small - Thumbnail size';
  SCptSizeClassMedium           = 'Medium - Document size';
  SCptSizeClassLarge            = 'Large - Desktop size';
  SCptInfoFmt                   = '%s %s %s, Processor %s, %d Cores';
  SLogErrorPrefix               = 'Error - ';
  SLogCantLoadProject           = 'Cant load ImageResize project File';
  SLogCantLoadSettings          = 'Cant load ImageResize settings File';
  SLogWarningSettingVersionFmt  = 'Warning: Unexpected settings format %s (%s expected)';
  SLogWarningProjectVersionFmt  = 'Warning: Unexpected project format %s (%s expected)';
  SCptFileNotFoundFmt           = 'File ''%s'' not found';
  SMsgProjectSavedToFmt         = 'Project saved to ''%s''';
  SMsgProjectLoadedFromFmt      = 'Project ''%s'' loaded';
  SCptSourceFilesFmt            = '%d source files';
  SErrMissingSourceFilenames    = 'Missing source filenames';
  SErrMissingSourceFolder       = 'Missing source folder';
  SErrMissingDestinationFolder  = 'Missing target folder';
  SErrInvalidSizes              = 'Invalid Sizes string';
  SErrInvalidJpgQuality         = 'Invalid JPEG quality';
  SErrInvalidMrkSize            = 'Invalid watermark size';
  SErrInvalidMrkXBrd            = 'Invalid watermark x border';
  SErrInvalidMrkYBrd            = 'Invalid watermark y border';
  SErrInvalidMrkOpacity         = 'Invalid watermark opacity';
  SErrEnterPlaceholder          = 'Enter placeholder %SIZE% to either the target folder or the file template';
  SErrAtFmt                     = 'Error at %.0f%% - %s';
  SErrCancelledAtFmt            = 'Cancelled at %.0f%%';
  SMsgCurrentDirFmt             = 'Current directory: %s';

{$R *.lfm}

function TMainDialog.HasFocus(Control :TWinControl) :boolean;
var
  Subject :TWinControl;
begin
  Subject := ActiveControl;
  while Subject<>nil do begin
    if Subject=Control then Exit(true);
    Subject := Subject.Parent;
  end;
  result := false;
end;

procedure MoveChecked(ActionList :TActionList; Delta :integer);
var
  i :integer;
  Action :TAction;
begin
  if Delta<0 then Delta := ActionList.ActionCount+Delta;
  for i:=0 to ActionList.ActionCount-1 do begin
    Action := ActionList.Actions[i] as TAction;
    if Action.Checked then begin
      TAction(ActionList.Actions[(i+Delta) mod ActionList.ActionCount]).Execute;
      exit;
    end;
  end;
end;

procedure SetRequiredState(Control :TLabel; Value :boolean);
begin
  if Value then begin
    Control.Font.Color := clRed;
  end else begin
    Control.Font.Color := clGreen;
  end;
end;

function StrToShuffleSeed(const Value :string) :integer;
begin
  if Value=SCptRandom then
    result := 0
  else
    if not TryStrToInt(Value, result) then raise Exception.Create(SErrInvalidShuffleSeed);
end;

function ShuffleSeedToStr(Value :integer) :string;
begin
  if Value<=0 then
    result := SCptRandom
  else
    result := IntToStr(Value);
end;

function IsSwitch(const ShortForm :Char; const LongForm :string) :boolean;
var
  Switch :string;
  i :integer;
begin
  for i:=1 to ParamCount do begin
    if (ParamStr(i)[1] = '-') or (ParamStr(i)[1] = '/') then begin
      Switch := Copy(ParamStr(i), 2, Length(ParamStr(i))-1);
      if (Switch=LongForm) or (Switch=ShortForm) then
        Exit(true);
    end;
  end;
  result := false;
end;

function IsSwitch(const ShortForm :Char; const LongForm :string; out Param :string) :boolean;
var
  Switch :string;
  i :integer;
begin
  for i:=1 to ParamCount-1 do begin
    if (ParamStr(i)[1] = '-') or (ParamStr(i)[1] = '/') then begin
      Switch := Copy(ParamStr(i), 2, Length(ParamStr(i))-1);
      if (Switch=ShortForm) or (Switch=LongForm) then begin
        Param := ParamStr(i+1).DeQuotedString('"');
        Exit(true);
      end;
    end;
  end;
  result := false;
end;

function GetNonSwitch :string;
begin
  if (ParamCount=0) or (ParamStr(1)[1]='-') then
    Exit('');
  result := ParamStr(1).DeQuotedString('"');
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
  LangCode :string;
  Resampling :TResampling;
begin
  LangCode := '';
  if (SysLocale.PriLangId=7) and not (IsSwitch('L', 'LANGUAGE', LangCode) and SameText(LangCode, 'en')) then
    SetDefaultLang('de');

  TLogger.DefaultLogger := TRichMemoLogger.Create(MemoMessages);

  AllowDropFiles := true;

  FProcessingSettings := TProcessingSettings.Create;
  FProcessingSettings.Defaults;

  FDialogSettings := TDialogSettings.Create;
  FDialogSettings.Defaults;

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
        Button.ImageIndex := IMAGEINDEX_IMGTHUMBNAIL
    else if DEFSIZES[i]<=DOCIMGMAX then
      Button.ImageIndex := IMAGEINDEX_IMGDOCUMENT
    else
      Button.ImageIndex := IMAGEINDEX_IMGSCREEN;
    Button.Style := tbsCheck;
    Button.Tag := DEFSIZES[i];
    Button.OnClick := @SizeButtonClick;
    Button.Visible := true;
  end;
  ToolBarSizeButtons.ButtonWidth := ToolBarSizeButtons.ClientWidth div 4 - 1;
  ToolBarSizeButtons.ButtonHeight := ToolBarSizeButtons.ClientHeight div 4;
  RadioButtonRenSimple.Hint := RENSIMPLETEMPLATE;
  RadioButtonRenAdvanced.Hint := RENADVANCEDTEMPLATE;
  for Resampling in TResampling do
    ComboBoxResampling.Items.Add(RESAMPLING_STRINGS[Resampling]);

  FWorkingDirectory := GetCurrentDir;
end;

procedure TMainDialog.FormShow(Sender: TObject);
var
  Filename :string;
  LocHelpDir :string;
begin

  // Show initial message in Message log
  Log(Format(SCptInfoFmt, [GUIVER_APP, GUIVER_VERSION, GUIVER_DATE, IMGRESVER, TThread.ProcessorCount]), llHint);

  LoadSettings;
  if not FDialogSettings.AutoSave or not LoadLastProject then
    ActionNew.Execute;
  RequiredStepsUpdate;

  FAutoExit := IsSwitch('X', 'AUTOEXIT');
  Filename := GetNonSwitch;
  if Filename<>'' then
    LoadProjectFromFile(Filename);
  if IsSwitch('A', 'AUTOSTART') then
    PostMessage(Handle, LM_RUN, 0, 0);

  // If no local help files, then use online help
  LocHelpDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+SLocDirHelp;
  if not DirectoryExists(LocHelpDir) then
    HTMLHelpDatabase.BaseURL := SUrlWebHelp
  else
    HTMLHelpDatabase.BaseURL := 'file://'+LocHelpDir;

  //with SysLocale do
  //  MemoMessages.Lines.Add(Format('DefaultLCID=%d PriLangID=%d SubLangID=%d', [DefaultLCID, PriLangID, SubLangID]));

  ActionParamSizes.Execute;
end;

procedure TMainDialog.LMRun(var Message: TLMessage);
begin
  ActionExecute.Execute;
  Application.ProcessMessages;
  if FAutoExit then
    Close;
end;

procedure TMainDialog.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckSave;
end;

procedure TMainDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if FDialogSettings.AutoSave then
    SaveLastProject;
  SaveSettings;
end;

procedure TMainDialog.FormDestroy(Sender: TObject);
begin
  FProcessingSettings.Free;
  FDialogSettings.Free;
end;

procedure TMainDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Delta :integer;
begin
  if (Key=VK_TAB) and (ssCtrl in Shift) then begin
    if ssShift in Shift then Delta := -1 else Delta := 1;
    if HasFocus(PanelSource) then
      MoveChecked(ActionListSource, Delta)
    else if HasFocus(PanelParams) then
      MoveChecked(ActionListParams, Delta);
  end;
end;

procedure TMainDialog.EditSrcFolderChange(Sender: TObject);
begin
  Dirty := true;
  UpdateRequiredStep(1);
end;

procedure TMainDialog.ActionParamExecute(Sender: TObject);
begin
  with Sender as TAction do begin
    Checked := true;
    PageControlParams.TabIndex := Tag;
    PageControlParams.SetFocus;
  end;
end;

procedure TMainDialog.ActionSettingsExecute(Sender: TObject);
begin
  SettingsDialog.SetProcessingSettings(FProcessingSettings);
  SettingsDialog.SetDialogSettings(FDialogSettings);
  if SettingsDialog.ShowModal = mrOk then begin
    SettingsDialog.GetProcessingSettings(FProcessingSettings);
    SettingsDialog.GetDialogSettings(FDialogSettings);
  end;
end;

procedure TMainDialog.ActionPresentationExecute(Sender: TObject);
begin
  PresentationDialog.ShowModal;
end;

procedure TMainDialog.ActionSourceExecute(Sender: TObject);
begin
  with Sender as TAction do begin
    Checked := true;
    PageControlSource.TabIndex := Tag;
    PageControlSource.SetFocus;
    UpdateRequiredStep(1);
    Dirty := true;
  end;
end;

procedure TMainDialog.ButtonBrowseTargetFolderClick(Sender: TObject);
var
  s :string;
  f :string;
begin
  s := EditTargetFolder.Text;
  f := '';
  if Pos('%SIZE%', s)>0 then
    f := ExtractFilename(s);
  BrowseTargetFolder.Filename := LeftStr(s, Length(s)-Length(f));
  if BrowseTargetFolder.Execute then
    EditTargetFolder.Text := IncludeTrailingPathDelimiter(BrowseTargetFolder.FileName)+f;
end;

procedure TMainDialog.ButtonBrowseMrkFilenameClick(Sender: TObject);
begin
  OpenDialogMrkFilename.Filename := EditMrkFilename.Text;
  if OpenDialogMrkFilename.Execute then begin
    EditMrkFilename.Text := OpenDialogMrkFilename.Filename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
  end;
end;

procedure TMainDialog.ButtonBrowseSrcFilesClick(Sender: TObject);
begin
  if OpenDialogSrcFilenames.Execute then
    MemoSrcFilenames.Lines.AddStrings(OpenDialogSrcFilenames.Files);
end;

procedure TMainDialog.ButtonBrowseSrcFolderClick(Sender: TObject);
begin
  BrowseSrcFolder.Filename := EditSrcFolder.Text;
  if BrowseSrcFolder.Execute then
    EditSrcFolder.Text := IncludeTrailingPathDelimiter(BrowseSrcFolder.FileName);
end;

procedure TMainDialog.ButtonClearCopyrightClick(Sender: TObject);
begin
  EditCopyRight.Text := '';
end;

procedure TMainDialog.ButtonClearSizesClick(Sender: TObject);
begin
  with EditSizes do if (SelLength>0) and (SelLength<Length(Text)) then
    SelText := ''
  else
    EditSizes.Text := '';
  UpdateRequiredStep(2);
end;

procedure TMainDialog.ButtonClearSrcFilesClick(Sender: TObject);
begin
  MemoSrcFilenames.Lines.Clear;
end;

procedure TMainDialog.ButtonInsertCopyrightClick(Sender: TObject);
begin
  EditCopyright.SelText := '© ';
end;

procedure TMainDialog.ButtonInsertSIZEClick(Sender: TObject);
begin
  EditTargetFolder.SelText := '%SIZE%';
end;

procedure TMainDialog.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log(SLogErrorPrefix+E.Message, llError);
end;

procedure TMainDialog.EditRenTemplateEnter(Sender: TObject);
begin
  RadioButtonRenCustom.Checked := true;
end;

procedure TMainDialog.ProjectChanged(Sender: TObject);
begin
  Dirty := true;
end;

procedure TMainDialog.ChangeCurrentDir(const Path: string);
begin
  SetCurrentDir(Path);
  Log(Format(SMsgCurrentDirFmt, [Path]), llHint);
end;

function TMainDialog.GetAppDataFilename(const Filetitle :string; CanCreate :boolean): string;
var
  Path, AppPath :string;
begin
  Path := IncludeTrailingPathDelimiter(GetWindowsSpecialDir(FOLDERID_LocalAppData, CanCreate));
  AppPath := IncludeTrailingPathDelimiter(Path+ChangeFileExt(ExtractFileName(Application.ExeName), '.'+GUIVER_VERSION));
  result := AppPath+Filetitle;
end;

procedure TMainDialog.SetDirty(AValue: boolean);
//const
//  INDICATORCOLORS :array[boolean] of TColor = (clDefault, clRed);
begin
  if FIsDirty=AValue then Exit;
  FIsDirty:=AValue;
  ActionSave.ImageIndex := IfThen(Dirty, IMAGEINDEX_DIRTY, IMAGEINDEX_SAVE);
//  PanelDirtyIndicator.Color := INDICATORCOLORS[Dirty];
end;

function TMainDialog.GetRequiredSteps(Index: integer): boolean;
begin
  result := FRequiredSteps[Index];
end;

procedure TMainDialog.SetRequiredSteps(Index : integer; AValue: boolean);
begin
  if FRequiredSteps[Index]<>AValue then begin
    FRequiredSteps[Index] := AValue;
    DoRequiredStepsChanged(Index);
  end;
end;

procedure TMainDialog.DoRequiredStepsChanged(Index: integer);
var
  Required :boolean;
begin
  case Index of
  1: PaintBoxStep1.Invalidate;
  2: PaintBoxStep2.Invalidate;
  3: PaintBoxStep3.Invalidate;
  end;
  for Required in FRequiredSteps do
    if Required then begin
      ActionExecute.Enabled := false;
      Exit;
    end;
  ActionExecute.Enabled := true;
end;

procedure TMainDialog.SaveSettings;
var
  Ini :TCustomIniFile;
begin
  Ini := TIniFile.Create(GetAppDataFilename(SETTINGS_FILENAME, true));
  with Ini do try
    WriteString(COMMONSECTION, 'Type', SETTYPE);
    WriteString(COMMONSECTION, 'Version', SETVERSION);
    Ini.WriteInteger(DIALOGSECTION, 'Width', Width);
    Ini.WriteInteger(DIALOGSECTION, 'Height', Height);
    Ini.WriteInteger(DIALOGSECTION, 'PanelControls.Height', PanelControls.Height);
    Ini.WriteString(DIALOGSECTION, 'CurrentDirectory', GetCurrentDir);
    Ini.WriteBool(DIALOGSECTION, 'AutoSave', FDialogSettings.AutoSave);
    Ini.WriteBool(DIALOGSECTION, 'WarnDirty', FDialogSettings.WarnDirty);
    EraseSection(SETTINGSSECTION);
    WriteBool(SETTINGSSECTION, 'StopOnError', FProcessingSettings.StopOnError);
    WriteInteger(SETTINGSSECTION, 'ThreadsUsed', FProcessingSettings.ThreadsUsed);
  finally
    Free;
  end;
end;

function TMainDialog.LoadSettings :boolean;
var
  Filename :string;
  Ini :TCustomIniFile;
  IniVer :string;
  IniTyp :string;
  AHeight :integer;
  Path :string;
begin
  Filename := GetAppDataFilename(SETTINGS_FILENAME, false);
  if not FileExists(Filename) then Exit(false);
  Ini := TIniFile.Create(Filename);
  with Ini do try
    IniTyp := Ini.ReadString('Common', 'Type', 'unknown');
    result := IniTyp=SETTYPE;
    if not result then begin
       Log(SLogCantLoadSettings, llWarning);
       Exit;
    end;
    IniVer := Ini.ReadString('Common', 'Version', '000');
    result := IniVer=SETVERSION;
    if not result then begin
      Log(Format(SLogWarningSettingVersionFmt, [IniVer, SETVERSION]), llWarning);
      Exit;
    end;
    Width := Ini.ReadInteger(DIALOGSECTION, 'Width', Width);
    Height := Ini.ReadInteger(DIALOGSECTION, 'Height', Height);
    AHeight := Ini.ReadInteger(DIALOGSECTION, 'PanelControls.Height', PanelControls.Height);
    if AHeight+PanelControls.Top + 16 < ClientHeight then
      PanelControls.Height := AHeight;
    FDialogSettings.AutoSave := Ini.ReadBool(DIALOGSECTION, 'AutoSave', FDialogSettings.AutoSave);
    FDialogSettings.WarnDirty := Ini.ReadBool(DIALOGSECTION, 'WarnDirty', FDialogSettings.WarnDirty);
    Path := Ini.ReadString(DIALOGSECTION, 'CurrentDirectory', GetCurrentDir);
    if DirectoryExists(Path) then
      ChangeCurrentDir(Path);
    FProcessingSettings.StopOnError := ReadBool(SETTINGSSECTION, 'StopOnError', FProcessingSettings.StopOnError);
    FProcessingSettings.ThreadsUsed := ReadInteger(SETTINGSSECTION, 'ThreadsUsed', FProcessingSettings.ThreadsUsed);
  finally
    Free;
  end;
end;

procedure TMainDialog.ActionNewExecute(Sender: TObject);
var
  ImgResizer :TProcessor;
begin
  if not CheckSave then Exit;
  MemoMessages.Lines.Clear;
  ImgResizer := TProcessor.Create;
  try
    MemoSrcFilenames.Lines.Clear;
    FProjectDescription                 := '';
    ActionSrcFilenames.Checked          := true;
    EditSrcFolder.Text                  := '';
    EditSrcMasks.Text                   := '*.jpg; *.png';
    EditTargetFolder.Text               := '';
    EditSizes.Text                      := '';
    ComboBoxJpgQuality.Text             := ImgResizer.JpgQualityToStr(ImgResizer.JpgQuality);
    ComboBoxPngCompression.Text         := TProcessor.PngCompressionToStr(ImgResizer.PngCompression);
    ComboBoxResampling.Text             := RESAMPLING_STRINGS[DEFAULT_RESAMPLING];
    EditMrkFilename.Text                := '';
    UpDownMrkSize.Position              := round(ImgResizer.MrkSize);
    UpDownMrkX.Position                 := round(ImgResizer.MrkX);
    UpDownMrkY.Position                 := round(ImgResizer.MrkY);
    UpDownMrkAlpha.Position             := round(ImgResizer.MrkAlpha);
    FIsSave                             := false;
    CheckBoxRenEnabled.Checked          := ImgResizer.RenEnabled;
    RadioButtonRenSimple.Checked        := true;
    EditRenTemplate.Text                := DEFAULT_RENFILETEMPLATE;
    CheckBoxShuffle.Checked             := DEFAULT_SHUFFLE;
    ComboBoxShuffleSeed.Text            := SCptRandom;
    CheckBoxMrkEnabled.Checked          := false;
    CheckBoxTagsSourceEXIF.Checked      := false;
    CheckBoxTagsSourceTagsFiles.Checked := false;
    CheckBoxTagTitle.Checked            := false;
    CheckBoxTagTimestamp.Checked        := false;
    CheckBoxTagCopyright.Checked        := false;
    EditCopyright.Text                  := '';
    CheckBoxTagsReportEnabled.Checked   := false;
    CheckBoxImageInfosEnabled.Checked   := false;
    CheckBoxNoCreate.Checked            := false;

    ActionSrcFilenames.Execute;
    ActionParamSizes.Execute;

    RequiredStepsUpdate;
    SetTitle(SCptUnnamed);
    FProjectFilename := '';
    ChangeCurrentDir(FWorkingDirectory);
    FIsSave := false;
    Dirty := false;
  finally
    ImgResizer.Free;
  end;
end;

function TMainDialog.LoadProjectFromIni(Ini :TCustomIniFile) :boolean;
var
  IniVer :string;
  IniTyp :string;
begin
  with Ini do begin
    IniTyp := Ini.ReadString('Common', 'Type', 'unknown');
    result := IniTyp=PRJTYPE;
    if not result then begin
       Log(SLogCantLoadProject, llWarning);
       Exit;
    end;
    IniVer := Ini.ReadString('Common', 'Version', '000');
    result := (IniVer=PRJVERSION) or (IniVer=PRJVERSION200) or (IniVer=PRJVERSION210);
    if not result then begin
      Log(Format(SLogWarningProjectVersionFmt, [IniVer, PRJVERSION]), llWarning);
      Exit;
    end;

    if SameText(ReadString(SETTINGSSECTION, 'Source', 'Filenames'), 'Filenames') then
      ActionSrcFilenames.Execute
    else
      ActionSrcFolder.Execute;
    FProjectDescription                   := ReadString(SETTINGSSECTION, 'Description', '');
    EditSrcFolder.Text                    := ReadString(SETTINGSSECTION, 'SourceFolder', EditSrcFolder.Text);
    EditSrcMasks.Text                     := ReadString(SETTINGSSECTION, 'SourceMasks', EditSrcMasks.Text);
    MemoSrcFilenames.Text                 := ReplaceStr(ReadString(SETTINGSSECTION, 'SourceFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP)), LINESEP, #13#10);
    EditTargetFolder.Text                 := ReadString(SETTINGSSECTION, 'TargetFolder', EditTargetFolder.Text);
    EditSizes.Text                        := ReadString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    RequiredStepsUpdate;
    ComboBoxJpgQuality.Text               := ReadString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    ComboBoxPngCompression.Text           := ReadString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    ComboBoxResampling.Text               := ReadString(SETTINGSSECTION, 'Resampling', RESAMPLING_STRINGS[DEFAULT_RESAMPLING]);
    CheckBoxMrkEnabled.Checked            := ReadBool(SETTINGSSECTION, 'MrkEnabled', CheckBoxMrkEnabled.Checked);
    EditMrkFilename.Text                  := ReadString(SETTINGSSECTION, 'MrkFilename', EditMrkFilename.Text);
    UpDownMrkSize.Position                := ReadInteger(SETTINGSSECTION, 'MrkSize', UpDownMrkSize.Position);
    UpDownMrkX.Position                   := ReadInteger(SETTINGSSECTION, 'MrkX', UpDownMrkX.Position);
    UpDownMrkY.Position                   := ReadInteger(SETTINGSSECTION, 'MrkY', UpDownMrkY.Position);
    UpDownMrkAlpha.Position               := ReadInteger(SETTINGSSECTION, 'MrkAlpha', UpDownMrkAlpha.Position);
    CheckBoxRenEnabled.Checked            := ReadBool(SETTINGSSECTION, 'RenEnabled', CheckBoxRenEnabled.Checked);
    RadioButtonRenSimple.Checked          := ReadBool(SETTINGSSECTION, 'RenSimple', RadioButtonRenSimple.Checked);
    RadioButtonRenAdvanced.Checked        := ReadBool(SETTINGSSECTION, 'RenAdvanced', RadioButtonRenAdvanced.Checked);
    RadioButtonRenCustom.Checked          := ReadBool(SETTINGSSECTION, 'RenCustom', RadioButtonRenCustom.Checked);
    EditRenTemplate.Text                  := ReadString(SETTINGSSECTION, 'RenTemplate', EditRenTemplate.Text);
    CheckBoxShuffle.Checked               := ReadBool(SETTINGSSECTION, 'Shuffle', CheckBoxShuffle.Checked);
    ComboBoxShuffleSeed.Text              := ShuffleSeedToStr(ReadInteger(SETTINGSSECTION, 'ShuffleSeed', 0));
    CheckBoxTagsSourceEXIF.Checked        := ReadBool(SETTINGSSECTION, 'TagsSourceEXIF', CheckBoxTagsSourceEXIF.Checked);
    CheckBoxTagsSourceTagsFiles.Checked   := ReadBool(SETTINGSSECTION, 'TagsSourceTagsFiles', CheckBoxTagsSourceTagsFiles.Checked);
    CheckBoxTagTitle.Checked              := ReadBool(SETTINGSSECTION, 'TagTitle', CheckBoxTagTitle.Checked);
    CheckBoxTagTimestamp.Checked          := ReadBool(SETTINGSSECTION, 'TagTimestamp', CheckBoxTagTimestamp.Checked);
    CheckBoxTagCopyright.Checked          := ReadBool(SETTINGSSECTION, 'TagCopyright', CheckBoxTagCopyright.Checked);
    EditCopyright.Text                    := ReadString(SETTINGSSECTION, 'Copyright', EditCopyright.Text);
    CheckBoxTagsReportEnabled.Checked     := ReadBool(SETTINGSSECTION, 'TagsReportEnabled', CheckBoxTagsReportEnabled.Checked);
    CheckBoxImageInfosEnabled.Checked     := ReadBool(SETTINGSSECTION, 'ImageInfosEnabled', CheckBoxImageInfosEnabled.Checked);
    CheckBoxNoCreate.Checked              := ReadBool(SETTINGSSECTION, 'NoCreate', DEFAULT_NOCREATE);
    ActionParamSizes.Execute;
  end;
end;

const
  SRCMODES :array[boolean] of string = ('Folder', 'Filenames');

procedure TMainDialog.SaveProjectToIni(Ini :TCustomIniFile);
begin
  // Navigate to proper "directory":
  with Ini do begin
    WriteString(COMMONSECTION, 'Type', PRJTYPE);
    WriteString(COMMONSECTION, 'Version', PRJVERSION);
    EraseSection(SETTINGSSECTION);
    WriteString(SETTINGSSECTION, 'Description', FProjectDescription);
    WriteString(SETTINGSSECTION, 'Source', SRCMODES[ActionSrcFilenames.Checked]);
    WriteString(SETTINGSSECTION, 'SourceFolder', EditSrcFolder.Text);
    WriteString(SETTINGSSECTION, 'SourceMasks', EditSrcMasks.Text);
    WriteString(SETTINGSSECTION, 'SourceFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP));
    WriteString(SETTINGSSECTION, 'Sizes', EditSizes.Text);
    WriteString(SETTINGSSECTION, 'TargetFolder', EditTargetFolder.Text);
    WriteString(SETTINGSSECTION, 'JpgOptions.Quality', ComboBoxJpgQuality.Text);
    WriteString(SETTINGSSECTION, 'PngOptions.Compression', ComboBoxPngCompression.Text);
    WriteString(SETTINGSSECTION, 'Resampling', ComboBoxResampling.Text);
    WriteBool(SETTINGSSECTION, 'MrkEnabled', CheckBoxMrkEnabled.Checked);
    WriteString(SETTINGSSECTION, 'MrkFilename', EditMrkFilename.Text);
    WriteString(SETTINGSSECTION, 'MrkSize', EditMrkSize.Text);
    WriteString(SETTINGSSECTION, 'MrkX', EditMrkX.Text);
    WriteString(SETTINGSSECTION, 'MrkY', EditMrkY.Text);
    WriteString(SETTINGSSECTION, 'MrkAlpha', EditMrkAlpha.Text);
    WriteBool(SETTINGSSECTION, 'RenEnabled', CheckBoxRenEnabled.Checked);
    WriteBool(SETTINGSSECTION, 'RenSimple', RadioButtonRenSimple.Checked);
    WriteBool(SETTINGSSECTION, 'RenAdvanced', RadioButtonRenAdvanced.Checked);
    WriteBool(SETTINGSSECTION, 'RenCustom', RadioButtonRenCustom.Checked);
    WriteString(SETTINGSSECTION, 'RenTemplate', EditRenTemplate.Text);
    WriteBool(SETTINGSSECTION, 'Shuffle', CheckBoxShuffle.Checked);
    WriteInteger(SETTINGSSECTION, 'ShuffleSeed', StrToShuffleSeed(ComboBoxShuffleSeed.Text));
    WriteBool(SETTINGSSECTION, 'TagsSourceEXIF', CheckBoxTagsSourceEXIF.Checked);
    WriteBool(SETTINGSSECTION, 'TagsSourceTagsFiles', CheckBoxTagsSourceTagsFiles.Checked);
    WriteBool(SETTINGSSECTION, 'TagTitle', CheckBoxTagTitle.Checked);
    WriteBool(SETTINGSSECTION, 'TagTimestamp', CheckBoxTagTimestamp.Checked);
    WriteBool(SETTINGSSECTION, 'TagCopyright', CheckBoxTagCopyright.Checked);
    WriteString(SETTINGSSECTION, 'Copyright', EditCopyright.Text);
    WriteBool(SETTINGSSECTION, 'TagsReportEnabled', CheckBoxTagsReportEnabled.Checked);
    WriteBool(SETTINGSSECTION, 'ImageInfosEnabled', CheckBoxImageInfosEnabled.Checked);
    WriteBool(SETTINGSSECTION, 'NoCreate', CheckBoxNoCreate.Checked);
  end;
end;

function TMainDialog.LoadLastProject: boolean;
var
  Ini :TIniFile;
  Filename :string;
begin
  Filename := GetAppDataFilename(LASTPROJECT_FILENAME, false);
  if not FileExists(Filename) then Exit(false);
  Ini := TIniFile.Create(Filename);
  try
    result := LoadProjectFromIni(Ini);
  finally
    Ini.Free;
  end;
  if result then begin
    FIsSave := false;
    Dirty := false;
    SetTitle(SCptLastProject);
  end;
end;

procedure TMainDialog.SaveLastProject;
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create(GetAppDataFilename(LASTPROJECT_FILENAME, true));
  try
    SaveProjectToIni(Ini);
  finally
    Ini.Free;
  end;
end;

function TMainDialog.LoadProjectFromFile(const Filename: string) :boolean;
var
  Ini :TIniFile;
begin
  if not FileExists(Filename) then
    raise Exception.CreateFmt(SCptFileNotFoundFmt, [Filename]);
  Ini := TIniFile.Create(Filename);
  try
    result := LoadProjectFromIni(Ini);
    if result then begin
      FProjectFilename := Filename;
      SetTitle(''''+Filename+'''');
      Log(Format(SMsgProjectLoadedFromFmt, [Filename]), llHint);
      if FProjectDescription<>'' then
        Log(Format(SMsgProjectDescriptionFmt, [FProjectDescription]), llNews);
      ChangeCurrentDir(ExtractFilePath(Filename));
      FIsSave := true;
      Dirty := false;
    end;
  finally
     Ini.Free;
  end;
end;

procedure TMainDialog.SaveProjectToFile(const Filename: string);
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  try
    SaveProjectToIni(Ini);
    FProjectFilename := Filename;
    SetTitle(''''+Filename+'''');
    ChangeCurrentDir(ExtractFilePath(Filename));
    Log(Format(SMsgProjectSavedToFmt, [Filename]), llHint);
    FIsSave := true;
    Dirty := false;
  finally
    Ini.Free;
  end;
end;

function TMainDialog.CheckSave: boolean;
var
  QueryResult :integer;
begin
  if FDialogSettings.WarnDirty and Dirty then begin
    QueryResult := Application.MessageBox(PChar(SMsgQuerySave), PChar(SCptQuerySave), MB_ICONQUESTION + MB_YESNOCANCEL);
    case QueryResult of
    IDYES:
      begin
        ActionSave.Execute;
        result := FIsSave;
      end;
    IDNO:
      result := true;
    IDCANCEL:
      result := false;
    else
      result := false;
    end;
  end else
    result := true;
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
  RequiredStepsUpdate;
end;

procedure TMainDialog.ActionOpenExecute(Sender: TObject);
begin
  if not CheckSave then Exit;
  if OpenDialog.Execute then begin
    LoadProjectFromFile(OpenDialog.Filename);
  end;
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

procedure TMainDialog.ActionSaveExecute(Sender: TObject);
begin
  if not FIsSave then
    ActionSaveAs.Execute
  else
    SaveProjectToFile(FProjectFilename);
end;

procedure TMainDialog.MemoSrcFilenamesChange(Sender: TObject);
begin
  UpdateRequiredStep(1);
  Dirty := true;
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

procedure TMainDialog.RequiredStepsUpdate;
var
  i :integer;
begin
  for i:=1 to 3 do
    UpdateRequiredStep(i);
end;

procedure TMainDialog.UpdateRequiredStep(Index :integer);
var
  Required :boolean;
  i :integer;
  SizeDict :TSizeDict;
  Sizes :TSizes;
begin
  case Index of
  1:
    begin
      Required := (ActionSrcFilenames.Checked and (Length(MemoSrcFilenames.Text) = 0)) or (ActionSrcFolder.Checked and (Length(EditSrcFolder.Text) = 0));
      RequiredSteps[1] := Required;
      LabelSourceFileListMessage.Caption := Format(SCptSourceFilesFmt, [MemoSrcFilenames.Lines.Count]);
    end;
  2:
    begin
      if TrySizesStrToSizes(EditSizes.Text, Sizes) then begin
        EditSizes.Text := SizesToSizesStr(Sizes);
        RequiredSteps[2] := Length(Sizes)=0;
      end else begin
        RequiredSteps[2] := true;
        if EditSizes.Focused then
          Warning(SErrInvalidSizes);
      end;
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
  3:
    RequiredSteps[3] :=  Length(EditTargetFolder.Text) = 0;
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

procedure TMainDialog.ProgressBarPaint(Sender: TObject);
var
  r :TRect;
begin
  r := ProgressBar.ClientRect;
  r.Right := round(r.Left + FProgress*(r.Width));
  with ProgressBar.Canvas do begin
    Brush.Color := STYLECOLOR_LIGHT2;
    FillRect(r);
  end;
end;

procedure TMainDialog.PaintBoxStep1Paint(Sender: TObject);
var
  PaintBox :TPaintBox;
begin
  PaintBox := Sender as TPaintBox;
  ImageList32x32.Draw(PaintBox.Canvas, 0, 0, IfThen(FRequiredSteps[PaintBox.Tag], IMAGEINDEX_REQUIRED, IMAGEINDEX_NOTREQUIRED));
end;

procedure TMainDialog.EditTargetFolderChange(Sender: TObject);
begin
  UpdateRequiredStep(3);
  Dirty := true;
end;

procedure TMainDialog.EditMrkSizeChange(Sender: TObject);
begin
  PaintBoxMrkPreview.Invalidate;
  Dirty := true;
end;

procedure TMainDialog.EditSizesExit(Sender: TObject);
begin
  UpdateRequiredStep(2);
end;

procedure TMainDialog.EditSizesKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then begin
    UpdateRequiredStep(2);
    Key := #0;
  end;
end;

procedure TMainDialog.EditSizesChange(Sender: TObject);
begin
  if not EditSizes.Focused then
    UpdateRequiredStep(2);
  Dirty := true;
end;

procedure TMainDialog.ActionSaveAsExecute(Sender: TObject);
begin
  SaveAsDialog.Filename := FProjectFilename;
  if SaveAsDialog.Execute then
    SaveProjectToFile(SaveAsDialog.Filename);
end;

procedure TMainDialog.ActionAboutExecute(Sender: TObject);
begin
  TAboutDialog.Execute(IMGRESGUICPR, SCptProcessor + ' ' +IMGRESVER, STxtLicense);
end;

procedure TMainDialog.ActionEditWatermarkExecute(Sender: TObject);
var
  MrkFilename :string;
begin
  if TMrkEditDialog.GetFilename(MrkFilename) then begin
    EditMrkFilename.Text := MrkFilename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
    PageControlParams.ActivePage := TabSheetWatermark;
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

procedure TMainDialog.OnPrint(Sender: TObject; const Line: string; Level :TLogLevel);
begin
  Log(Line, Level);
  Application.ProcessMessages;
  if FCancelled then
    (Sender as TProcessor).Cancel;
end;

procedure TMainDialog.OnProgress(Sender: TObject; Progress: single);
begin
  FProgress := Progress;
  ProgressBar.Invalidate;
//  ProgressBar.Position := round(Progress*100.0);
  Application.ProcessMessages;
//  Sleep(500);
  if FCancelled then
    (Sender as TProcessor).Cancel;
end;

procedure TMainDialog.ActionExecuteExecute(Sender: TObject);
var
  Sizes :TIntegerDynArray;
  x :single;
  p :TPos;
  TargetFolder :string;
  IntValue :integer;
  SourceFilenames :TStringList;
  TagsSources :TTagsSources;
  TagsReports :TTagsReports;
  TagKeys :TStringArray;
  Processor :TProcessor;
begin
  if FExecuting then begin
    FCancelled := true;
    ActionExecute.Enabled := false;
  end else begin
    FProgress := 0;
    ProgressBar.Invalidate;
    ActionExecute.Enabled := true;
    ActionExecute.Caption := SCptCancel;
    ButtonExecute.Caption := SCptCancel;
    ActionExecute.ImageIndex := 6;
    ButtonExecute.ImageIndex := 6;
    ButtonExecute.Invalidate;
    ProgressBar.Visible := true;
    TimerProgressBarOff.Enabled := false;
    MemoMessages.Lines.Clear;
    FExecuting := true;
    FCancelled := false;
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    Processor := nil;
    try
      SourceFilenames := TStringList.Create;
      try
        // Source filenames
        if ActionSrcFilenames.Checked then begin
          if (MemoSrcFilenames.Text='') then
            raise Exception.Create(SErrMissingSourceFilenames);
          SourceFilenames.Assign(MemoSrcFilenames.Lines);
        end else begin
          if (EditSrcFolder.Text='') or not DirectoryExists(EditSrcFolder.Text) then
              raise Exception.Create(SErrMissingSourceFolder);
          FindAllFiles(SourceFilenames, EditSrcFolder.Text, EditSrcMasks.Text, false);
        end;

        // Destination folder
        if (EditTargetFolder.Text='') then
          raise Exception.Create(SErrMissingDestinationFolder);

        // Sizes
        Sizes := nil;
        if EditSizes.Text = SCptSizesDefault then begin
          SetLength(Sizes, 1);
          Sizes[0] := DEFAULTSIZE;
        end else if not TrySizesStrToSizes(EditSizes.Text, Sizes) then
          raise Exception.Create(SErrInvalidSizes);

        // Quality
        Processor := TProcessor.Create;
        if not TProcessor.TryStrToJpgQuality(ComboBoxJpgQuality.Text, IntValue) then
          raise Exception.Create(SErrInvalidJpgQuality);
        Processor.JpgQuality := IntValue;
        Processor.PngCompression := ComboBoxPngCompression.ItemIndex;
        Processor.Resampling := TResampling(ComboBoxResampling.ItemIndex);

        // Rename
        if CheckBoxRenEnabled.Checked then begin
          if RadioButtonRenSimple.Checked then
            Processor.TargetFiletemplate := RENSIMPLETEMPLATE
          else if RadioButtonRenAdvanced.Checked then
            Processor.TargetFiletemplate := RENADVANCEDTEMPLATE
          else
            Processor.TargetFiletemplate := EditRenTemplate.Text;
        end else
          Processor.TargetFiletemplate := '';
        Processor.Shuffle := CheckBoxShuffle.Checked;
        Processor.ShuffleSeed := StrToShuffleSeed(ComboBoxShuffleSeed.Text);

        // Watermark
        Processor.MrkFilename := EditMrkFilename.Text;

        if not TryStrToFloat(EditMrkSize.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkSize);
        Processor.MrkSize := x;

        if not TryStrToFloat(EditMrkX.Text, p.x) or (p.x<0.0) or (p.x>100.0) then
          raise Exception.Create(SErrInvalidMrkXBrd);
        if not TryStrToFloat(EditMrkY.Text, p.y) or (p.y<0.0) or (p.y>100.0) then
          raise Exception.Create(SErrInvalidMrkYBrd);

        Processor.MrkX := StrToFloat(EditMrkX.Text);
        Processor.MrkY := StrToFloat(EditMrkY.Text);

        if not TryStrToFloat(EditMrkAlpha.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkOpacity);
        Processor.MrkAlpha := x;

        // Tagging
        TagKeys := nil;
        TagsSources := [];
        if CheckBoxTagsSourceTagsFiles.Checked then Include(TagsSources, tsTagsFiles);
        if CheckBoxTagsSourceEXIF.Checked then Include(TagsSources, tsEXIF);
        Processor.TagsSources := TagsSources;
        if CheckBoxTagTitle.Checked then TagKeys.Add(TAGID_TITLE);
        if CheckBoxTagTimestamp.Checked then TagKeys.Add(TAGID_TIMESTAMP);
        if CheckBoxTagCopyright.Checked then TagKeys.Add(TAGID_COPYRIGHT);
        Processor.TagKeys := TagKeys;
        if CheckBoxTagCopyright.Checked then
          Processor.Copyright := EditCopyright.Text
        else
          Processor.Copyright := '';
        TagsReports := [];
        if CheckBoxTagsReportEnabled.Checked then include(TagsReports, trTagsReport);
        if CheckBoxImageInfosEnabled.Checked then include(TagsReports, trImages);
        Processor.TagsReports := TagsReports;

        // NoCreate flag
        Processor.NoCreate := CheckBoxNoCreate.Checked;

        // Hook the processor
        Processor.OnPrint := @OnPrint;
        Processor.OnProgress := @OnProgress;

        // warn, if %SIZE% placeholder is not contained either in
        // TargetFolder nor in FileTemplate
        TargetFolder := EditTargetFolder.Text;
        if (Length(Sizes)>1) and (Pos('%SIZE%', TargetFolder)=0)
         and not (Processor.RenEnabled and (Pos('%SIZE%', Processor.TargetFiletemplate)>0)) then
          raise Exception.Create(SErrEnterPlaceholder);

        Processor.Sizes := SizesToSizesStr(Sizes);
        Processor.SourceFilenames := SourceFilenames;
        Processor.TargetFolder := TargetFolder;
        Processor.ThreadCount := FProcessingSettings.ThreadsUsed;
        Processor.StopOnError := FProcessingSettings.StopOnError;
        Processor.Execute;

      except on E :Exception do
        begin
          Log(Format(SErrAtFmt, [FProgress*100.0, E.Message]), llError);
        end;
      end;
    finally
      SourceFilenames.Free;
      Processor.Free;
      FExecuting := false;
      if FCancelled then Log(Format(SErrCancelledAtFmt, [FProgress*100.0]), llWarning);
      ActionExecute.Enabled := true;
      ActionExecute.Caption := SCptExecuteA;
      ButtonExecute.Caption := SCptExecute;
      ActionExecute.ImageIndex := 5;
      ButtonExecute.ImageIndex := 5;
      TimerProgressBarOff.Enabled := true;
      Screen.Cursor := crDefault;
    end;
  end;

end;

procedure TMainDialog.ActionHelpExecute(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('','HTML/index.html');
//  OpenUrl('http://www.atomek.de/imageresize/index.html#gui');
end;

end.

