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

{$mode delphi}

interface

uses
  Windows, LCLTranslator, Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ActnList, ExtCtrls, imgres, AboutDlg, IniFiles, StrUtils,
  LMessages, LCLIntf, Buttons, ImgList, LCLType, LazHelpHTML, CheckLst,
  BGRABitmap, BGRABitmapTypes, BGRASpeedButton, BGRAGraphicControl, RichMemo,
  Generics.Collections, MrkEditDlg, WinDirs, UpdateUtils, AppSettings, Logging,
  LoggingRichMemo, StringArrays, Presentations, PresentationDlg, Settings,
  TagIds, LazFileUtils, Translations, Language, ControlShot, IniFilesHelper,
  ImagesMod;

const
  LM_RUN                = LM_USER + 1;

  GUIVER_APP            = 'ImageResize';
  GUIVER_VERSION        = '4.1';
  GUIVER_DATE           = '2024-02-24';

  GUIVER                :TVersionManifest = (App: GUIVER_APP; Version: GUIVER_VERSION; Date: GUIVER_DATE; Hint: '');

  APPWEBURL             = 'www.atomek.de/imageresize/index.html';
  APPDOWNLOADURL        = 'www.atomek.de/imageresize/download/InstallImageResize.zip';
  APPGITHUBURL          = 'https://github.com/Atomek61/ImageResize';
  GUIVERURL             = 'https://www.atomek.de/imageresize/download/version.manifest';

  IMGRESGUICPR          =  GUIVER_APP+' '+GUIVER_VERSION+' © 2024 Jan Schirrmacher, www.atomek.de';

  PRJTYPE               = 'IRS';
  PRJVERSION200         = '200';
  PRJVERSION210         = '210';
  PRJVERSION300         = '300';
  PRJVERSION            = '400';

  SETTYPE               = 'IST';
  SETVERSION            = '100';

  LASTPROJECT_FILENAME  = 'lastproject.irs';
  SETTINGS_FILENAME     = 'settings.ini';

  COMMON_SECTION        = 'Common';
  PROJECT_SECTION       = 'Project';
  PROCESSING_SECTION    = 'Processor';
  MAINDIALOG_SECTION    = 'MainDialog';
  PRESDIALOG_SECTION    = 'PresentationDialog';

  RENSIMPLETEMPLATE     = 'img{INDEX:1,3}.{FILEEXT}';
  RENADVANCEDTEMPLATE   = 'img{INDEX:1,3}_{SIZE}.{FILEEXT}';
  DEFAULT_SRCMASK       = '*.jpg;*.png';

  SIZEBTNHINTFMT        = '%s - %dpx';

  MRKRECTRATIO          = 3.0;
  LINESEP               = '|';

  IMGIDX_START          = 5;
  IMGIDX_CANCEL         = 6;
  IMGIDX_DIRTY          = 4;
  IMGIDX_SAVE           = 2;
  IMGIDX_IMGTHUMBNAIL   = 9;
  IMGIDX_IMGDOCUMENT    = 10;
  IMGIDX_IMGSCREEN      = 11;
  IMGIDX_REQUIRED       = 22;
  IMGIDX_NOTREQUIRED    = 23;

  clOrange              = $3d69a6;
  clDarkGray            = $403040;

  STYLECOLOR_LIGHT      = $00F1DDC9;
  STYLECOLOR_LIGHT2     = $00E6C09B;
  STYLECOLOR_DARK       = $00D59453;

  LOGCOLORS :array[llHint..llCrash] of TColor = (clGray, clDarkGray, clGreen, clOrange, clMaroon, clRed);

resourcestring
  SCptDependenciesFmt = 'Build with Lazarus %s, BGRABitmap %s, dExif %s, RichMemo';
  SUrlWebHelp = 'http://www.atomek.de/imageresize/hlp40/gui/en';
  SLocDirHelp = 'hlp\gui\en';

type

  { TMainDialog }

//  TSizeDict = specialize TDictionary<integer, integer>;

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TSizeInfo = record
    Size :integer;
    Name :string;
    Enabled :boolean;
  end;

  TSizeInfos = array of TSizeInfo;

  TMainDialog = class(TForm)
    ActionHelpScreenshots: TAction;
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
    BitBtn1: TBitBtn;
    ButtonSizeAdd: TButton;
    ComboBoxSize: TComboBox;
    ComboBoxSizeName: TComboBox;
    ImageStep1: TImage;
    ImageStep2: TImage;
    ImageStep3: TImage;
    Label2: TLabel;
    LabelRenTemplate: TLabel;
    ListBoxSizes: TListBox;
    PaintBoxMrkPreview: TBGRAGraphicControl;
    ButtonExecute: TBGRASpeedButton;
    CheckBoxDryRun: TCheckBox;
    CheckBoxImageInfosEnabled: TCheckBox;
    Image: TImage;
    LabelStep1: TLabel;
    LabelStep2: TLabel;
    LabelStep3: TLabel;
    MainActionList: TActionList;
    ApplicationProperties1: TApplicationProperties;
    BrowseSrcFolder: TSelectDirectoryDialog;
    ButtonBrowseMrkFilename: TBitBtn;
    ButtonBrowseSrcFolder: TBitBtn;
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
    ComboBoxInterpolation: TComboBox;
    ComboBoxShuffleSeed: TComboBox;
    EditCopyright: TEdit;
    EditSrcFolder: TEdit;
    EditSrcMasks: TEdit;
    EditRenTemplate: TComboBox;
    ComboBoxJPEGQuality: TComboBox;
    ComboBoxPNGCompression: TComboBox;
    EditMrkOpacity: TEdit;
    EditMrkFilename: TEdit;
    EditMrkSize: TEdit;
    EditMrkX: TEdit;
    EditMrkY: TEdit;
    EditTargetFolder: TEdit;
    GroupBoxInterpolationQuality: TGroupBox;
    GroupBoxTaggingSave: TGroupBox;
    GroupBoxTaggingLoad: TGroupBox;
    GroupBoxTaggingEXIF: TGroupBox;
    GroupBoxMrkFilename: TGroupBox;
    GroupBoxShuffle: TGroupBox;
    GroupBoxRename: TGroupBox;
    GroupBoxMrkLayout: TGroupBox;
    GroupBoxJPEGQuality: TGroupBox;
    GroupBoxPNGCompression: TGroupBox;
    HTMLBrowserHelpViewer: THTMLBrowserHelpViewer;
    HTMLHelpDatabase: THTMLHelpDatabase;
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
    LabelShuffleSeed: TLabel;
    Label18: TLabel;
    Label20: TLabel;
    Label4: TLabel;
    LabelSourceFileListMessage: TLabel;
    Label9: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LabelMrkSpace: TLabel;
    MemoSrcFilenames: TMemo;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PageControlSource: TPageControl;
    PageControlParams: TPageControl;
    PanelPreview: TPanel;
    ProgressBar: TPaintBox;
    PanelTarget: TPanel;
    PanelParams: TPanel;
    PanelSource: TPanel;
    PanelControls: TPanel;
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
    ToolButtonSep2: TToolButton;
    ToolButtonSep1: TToolButton;
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
    ToolButtonExecute: TToolButton;
    ToolButtonHelp: TToolButton;
    ToolButtonSep0: TToolButton;
    ToolButtonEditor: TToolButton;
    UpDownMrkOpacity: TUpDown;
    UpDownMrkSize: TUpDown;
    UpDownMrkX: TUpDown;
    UpDownMrkY: TUpDown;
    procedure ActionHelpScreenshotsExecute(Sender: TObject);
    procedure ActionSettingsExecute(Sender: TObject);
    procedure ActionPresentationExecute(Sender: TObject);
    procedure ActionSourceExecute(Sender: TObject);
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonBrowseMrkFilenameClick(Sender: TObject);
    procedure ButtonBrowseSrcFilesClick(Sender: TObject);
    procedure ButtonBrowseSrcFolderClick(Sender: TObject);
    procedure ButtonClearCopyrightClick(Sender: TObject);
    procedure ButtonClearSrcFilesClick(Sender: TObject);
    procedure ButtonInsertCopyrightClick(Sender: TObject);
    procedure ButtonInsertSIZEClick(Sender: TObject);
    procedure ActionParamExecute(Sender :TObject);
    procedure ComboBoxSizeDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
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
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure ApplicationProperties1Exception(Sender: TObject; E: Exception);
    procedure ListBoxSizesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
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
    procedure TimerProgressBarOffTimer(Sender: TObject);
    procedure ProjectChanged(Sender :TObject);
  private
    // Project settings. Most of the settings are stored in the MainDialog controls.
    FProjectFilename :string;
    FProjectDescription :string; // From project file
    FPresentationSettings :TPresentationSettings;
    FPresentationParamsList :TSettingsList;
  private
    // Application
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
    FSizeInfos :TSizeInfos;
    procedure ChangeCurrentDir(const Path :string);
    function GetAppDataFilename(const Filetitle :string; CanCreate :boolean) :string;
    procedure SetDirty(AValue: boolean);
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string; Level :TLogLevel);
    procedure OnProgress(Sender :TObject; Progress :single);
    function LoadSettings :boolean;
    procedure SaveSettings;
    procedure InitProject;
    function LoadProjectFromIni(Ini :TCustomIniFile) :boolean;
    procedure SaveProjectToIni(Ini :TCustomIniFile; SavePathesAsIs :boolean);
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
    function MouseToSpace(X, Y :integer) :TSize;
    function MouseToMrkSpace(X, Y :integer; out Value :TSize) :boolean;
    function CalcMarkRect(out Rect :TRect) :boolean;
    function HasFocus(Control :TWinControl) :boolean;
    function StrToSizeInfos(const Sizes, SizeNames :string) :TSizeInfos;
    procedure SizeInfosToStr(const SizeInfos :TSizeInfos; out SizeStr :string; out SizeNames :string);
  public
    property RequiredSteps[Index :integer] :boolean read GetRequiredSteps write SetRequiredSteps;
    property Dirty :boolean read FIsDirty write SetDirty;
  end;

var
  MainDialog: TMainDialog;

implementation

uses
  math, Utils, helpintfs, FileUtil, SettingsDlg;

const
  SCptSizesDefault  = 'default';

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
  SErrMissingSourceFolder       = 'Source folder not found';
  SErrMissingDestinationFolder  = 'Missing target folder';
  SErrInvalidSizes              = 'Invalid Sizes string';
  SErrInvalidJpgQuality         = 'Invalid JPEG quality';
  SErrInvalidMrkSize            = 'Invalid watermark size';
  SErrInvalidMrkXBrd            = 'Invalid watermark x border';
  SErrInvalidMrkYBrd            = 'Invalid watermark y border';
  SErrInvalidMrkOpacity         = 'Invalid watermark opacity';
  SErrEnterPlaceholder          = 'Enter placeholder {SIZE} or {SIZENAME} to either the target folder or the file template';
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

function TMainDialog.StrToSizeInfos(const Sizes, SizeNames: string): TSizeInfos;
var
  s :TStringArray;
  Item :String;
  Size :integer;
  i :integer;
begin
  result := nil;
  s := Sizes.Split(',');
  for Item in s do begin
    SetLength(result, Length(result)+1);
    result[High(result)].Enabled := true;
    if not TryStrToInt(Item, Size) then
      raise Exception.Create(SErrInvalidSizes);
    result[High(result)].Size := Size;
  end;
  s := SizeNames.Split(',');
  for i:=0 to High(result) do begin
    if i>=Length(s) then Exit;
    result[i].Name := s[i];
  end;
end;

procedure TMainDialog.SizeInfosToStr(const SizeInfos: TSizeInfos; out SizeStr: string; out SizeNames: string);
var
  i :integer;
begin
  SizeStr := '';
  for i:=0 to High(SizeInfos) do
    SizeStr := SizeStr + IfThen(i>0, ', ', '') + IntToStr(SizeInfos[i].Size);
  SizeNames := '';
  for i:=0 to High(SizeInfos) do
    SizeNames := SizeNames + IfThen(i>0, ', ', '') + SizeInfos[i].Name;
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
  if Value=SCptRandomSeed then
    result := 0
  else
    if not TryStrToInt(Value, result) then raise Exception.Create(SErrInvalidShuffleSeed);
end;

function ShuffleSeedToStr(Value :integer) :string;
begin
  if Value<=0 then
    result := SCptRandomSeed
  else
    result := IntToStr(Value);
end;

function IsSwitch(const ShortForm :Char; const LongForm :string) :boolean; overload;
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

function IsSwitch(const ShortForm :Char; const LongForm :string; out Param :string) :boolean; overload;
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
  LangCode, ExpliciteLangCode :string;
  Interpolation :TInterpolation;
  Item :string;
begin
  if IsSwitch('L', 'LANGUAGE', ExpliciteLangCode) then
    LangCode := LowerCase(ExpliciteLangCode)
  else
    LangCode := LowerCase(GetLanguageId.CountryCode);
  TLanguage.SetCode(LangCode);
  SetDefaultLang(TLanguage.Code);

  TLogger.DefaultLogger := TRichMemoLogger.Create(MemoMessages);

  AllowDropFiles := true;

  FProcessingSettings := TProcessingSettings.Create;
  FProcessingSettings.SetDefaults;

  FDialogSettings := TDialogSettings.Create;
  FDialogSettings.SetDefaults;

  FPresentationSettings := TPresentationSettings.Create;
  FPresentationParamsList := TSettingsList.Create;

{$IFDEF _DEBUG}
  ActionHelpScreenshots.Visible := True;
{$ENDIF}

  RadioButtonRenSimple.Hint := RENSIMPLETEMPLATE;
  RadioButtonRenAdvanced.Hint := RENADVANCEDTEMPLATE;

  for i in DEFSIZES do
    ComboBoxSize.Items.Add(IntToStr(i));

  for Interpolation in TInterpolation do
    ComboBoxInterpolation.Items.Add(INTERPOLATION_STRINGS[Interpolation]);

  ComboBoxJPEGQuality.Items.Clear;
  for Item in JPEGQUALITY_STRINGS do
    ComboBoxJPEGQuality.Items.Add(Item);

  ComboBoxPNGCompression.Items.Clear;
  for Item in PNGCOMPRESSION_STRINGS do
    ComboBoxPNGCompression.Items.Add(Item);

  ComboBoxShuffleSeed.Items[0] := SCptRandomSeed;

  FWorkingDirectory := GetCurrentDir;
end;

procedure TMainDialog.FormShow(Sender: TObject);
var
  Filename :string;
  LocHelpDir :string;
begin

  LoadSettings;
  ActionNew.Execute;

  if FDialogSettings.AutoSave.Value then
    LoadLastProject;

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
  if FDialogSettings.AutoSave.Value then
    SaveLastProject;
  SaveSettings;
end;

procedure TMainDialog.FormDestroy(Sender: TObject);
begin
  FProcessingSettings.Free;
  FDialogSettings.Free;
  FPresentationSettings.Free;
  FPresentationParamsList.Free;
end;

procedure TMainDialog.SaveSettings;
var
  Ini :TCustomIniFile;
begin
  Ini := TIniFile.Create(GetAppDataFilename(SETTINGS_FILENAME, true));
  with Ini do try
    WriteString(COMMON_SECTION, 'Type', SETTYPE);
    WriteString(COMMON_SECTION, 'Version', SETVERSION);

    Ini.WriteInteger(MAINDIALOG_SECTION, 'Width', Width);
    Ini.WriteInteger(MAINDIALOG_SECTION, 'Height', Height);
    Ini.WriteInteger(MAINDIALOG_SECTION, 'Left', Left);
    Ini.WriteInteger(MAINDIALOG_SECTION, 'Top', Top);
    Ini.WriteInteger(MAINDIALOG_SECTION, 'PanelControls.Height', PanelControls.Height);
    Ini.WriteString(MAINDIALOG_SECTION, 'CurrentDirectory', GetCurrentDir);

    FDialogSettings.Save(Ini, MAINDIALOG_SECTION);

    with PresentationDialog do begin
      Ini.WriteInteger(PRESDIALOG_SECTION, 'Top', Top);
      Ini.WriteInteger(PRESDIALOG_SECTION, 'Left', Left);
      Ini.WriteInteger(PRESDIALOG_SECTION, 'Width', Width);
      Ini.WriteInteger(PRESDIALOG_SECTION, 'Height', Height);
      Ini.WriteInteger(PRESDIALOG_SECTION, 'PanelMain.Height', PanelMain.Height);
    end;

    FProcessingSettings.Save(Ini, PROCESSING_SECTION);
  finally
    Free;
  end;
end;

procedure TMainDialog.InitProject;
var
  ImgResizer :TProcessor;
begin
  ImgResizer := TProcessor.Create;
  try
    MemoSrcFilenames.Lines.Clear;
    FProjectDescription                 := '';
    ActionSrcFilenames.Checked          := true;
    EditSrcFolder.Text                  := '';
    EditSrcMasks.Text                   := DEFAULT_SRCMASK;
    ListBoxSizes.Items.Clear;
    EditTargetFolder.Text               := '';
    ComboBoxJPEGQuality.ItemIndex       := 0;
    ComboBoxPNGCompression.ItemIndex    := 0;
    ComboBoxInterpolation.ItemIndex     := 0;
    EditMrkFilename.Text                := '';
    with ImgResizer.WatermarkParams do begin
      UpDownMrkSize.Position            := round(Size);
      UpDownMrkX.Position               := round(X);
      UpDownMrkY.Position               := round(Y);
      UpDownMrkOpacity.Position           := round(Opacity);
    end;
    FIsSave                             := false;
    CheckBoxRenEnabled.Checked          := ImgResizer.RenEnabled;
    RadioButtonRenSimple.Checked        := true;
    EditRenTemplate.Text                := DEFAULT_RENFILETEMPLATE;
    CheckBoxShuffle.Checked             := DEFAULT_SHUFFLE;
    ComboBoxShuffleSeed.Text            := SCptRandomSeed;
    CheckBoxMrkEnabled.Checked          := false;
    CheckBoxTagsSourceEXIF.Checked      := false;
    CheckBoxTagsSourceTagsFiles.Checked := false;
    CheckBoxTagTitle.Checked            := false;
    CheckBoxTagTimestamp.Checked        := false;
    CheckBoxTagCopyright.Checked        := false;
    EditCopyright.Text                  := '';
    CheckBoxImageInfosEnabled.Checked   := true;
    CheckBoxTagsReportEnabled.Checked   := false;
    CheckBoxDryRun.Checked            := false;

    ActionSrcFilenames.Execute;
    ActionParamSizes.Execute;

    FPresentationSettings.SetDefaults;
    FPresentationParamsList.Clear;

    RequiredStepsUpdate;
    SetTitle(SCptUnnamed);
    FProjectFilename := '';
    FIsSave := false;
    Dirty := false;

    FPresentationSettings.SetDefaults;
    FPresentationParamsList.Clear;

    // Show initial message in Message log
    MemoMessages.Clear;
    Log(Format(SCptInfoFmt, [GUIVER_APP, GUIVER_VERSION, GUIVER_DATE, IMGRESVER, TThread.ProcessorCount]), llNews);

  finally
    ImgResizer.Free;
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

    Top     := Ini.ReadInteger(MAINDIALOG_SECTION, 'Top', Top);
    Left    := Ini.ReadInteger(MAINDIALOG_SECTION, 'Left', Left);
    Width   := Ini.ReadInteger(MAINDIALOG_SECTION, 'Width', Width);
    Height  := Ini.ReadInteger(MAINDIALOG_SECTION, 'Height', Height);
    AHeight := Ini.ReadInteger(MAINDIALOG_SECTION, 'PanelControls.Height', PanelControls.Height);
    if AHeight+PanelControls.Top + 16 < ClientHeight then
      PanelControls.Height := AHeight;
    Path := Ini.ReadString(MAINDIALOG_SECTION, 'CurrentDirectory', GetCurrentDir);
    if DirectoryExists(Path) then
      ChangeCurrentDir(Path);
    FDialogSettings.Load(Ini, MAINDIALOG_SECTION, lmStrict);

    with PresentationDialog do begin
      Top     := Ini.ReadInteger(PRESDIALOG_SECTION, 'Top', Top);
      Left    := Ini.ReadInteger(PRESDIALOG_SECTION, 'Left', Left);
      Width   := Ini.ReadInteger(PRESDIALOG_SECTION, 'Width', Width);
      Height  := Ini.ReadInteger(PRESDIALOG_SECTION, 'Height', Height);
      AHeight := Ini.ReadInteger(PRESDIALOG_SECTION, 'PanelMain.Height', PanelMain.Height);
      if AHeight+PanelMain.Top + 16 < ClientHeight then
        PanelMain.Height := AHeight;
    end;

    FProcessingSettings.Load(Ini, PROCESSING_SECTION);
  finally
    Free;
  end;
end;

function TMainDialog.LoadProjectFromIni(Ini :TCustomIniFile) :boolean;
var
  IniVer :string;
  IniTyp :string;
  i, Value :integer;
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

    InitProject;

    FProjectDescription                   := ReadLang(PROJECT_SECTION,  'Description', '', TLanguage.Code);
    if SameText(ReadString(PROJECT_SECTION, 'Source', 'Filenames'), 'Filenames') then
      ActionSrcFilenames.Execute
    else
      ActionSrcFolder.Execute;
//    ActionSrcFilenames.Checked            := SameText(ReadString(PROJECT_SECTION, 'Source', 'Filenames'), 'Filenames');
    EditSrcFolder.Text                    := ReadString(PROJECT_SECTION,  'SourceFolder', EditSrcFolder.Text);
    EditSrcMasks.Text                     := ReadString(PROJECT_SECTION,  'SourceMasks', EditSrcMasks.Text);
    MemoSrcFilenames.Text                 := ReplaceStr(ReadString(PROJECT_SECTION, 'SourceFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, LINESEP)), LINESEP, #13#10);
    FSizeInfos                            := StrToSizeInfos(ReadString(PROJECT_SECTION,  'Sizes', ''), ReadString(PROJECT_SECTION,  'SizeNames', ''));
    ListBoxSizes.Items.Clear;
    for i:=0 to High(FSizeInfos) do ListBoxSizes.Items.Add(IntToStr(FSizeInfos[i].Size));
    EditTargetFolder.Text                 := ReadString(PROJECT_SECTION,  'TargetFolder', EditTargetFolder.Text);
    RequiredStepsUpdate;
    ComboBoxInterpolation.ItemIndex       := integer(TProcessor.NameToInterpolation(ReadString(PROJECT_SECTION,  'Interpolation', INTERPOLATION_NAMES[DEFAULT_INTERPOLATION])));
    Value := ReadInteger(PROJECT_SECTION,  'JPEGQuality', TProcessor.StrToJPEGQuality(ComboBoxJPEGQuality.Text));
    if Value = DEFAULTJPEGQUALITY then ComboBoxJPEGQuality.ItemIndex := 0 else ComboBoxJPEGQuality.Text := IntToStr(Value);
    ComboBoxPNGCompression.ItemIndex      := TProcessor.NameToPNGCompression(ReadString(PROJECT_SECTION,  'PNGCompression', PNGCOMPRESSION_NAMES[ComboBoxPNGCompression.ItemIndex]));
    CheckBoxMrkEnabled.Checked            := ReadBool(PROJECT_SECTION,    'MrkEnabled', CheckBoxMrkEnabled.Checked);
    EditMrkFilename.Text                  := ReadString(PROJECT_SECTION,  'MrkFilename', EditMrkFilename.Text);
    UpDownMrkSize.Position                := ReadInteger(PROJECT_SECTION, 'MrkSize', UpDownMrkSize.Position);
    UpDownMrkX.Position                   := ReadInteger(PROJECT_SECTION, 'MrkX', UpDownMrkX.Position);
    UpDownMrkY.Position                   := ReadInteger(PROJECT_SECTION, 'MrkY', UpDownMrkY.Position);
    UpDownMrkOpacity.Position             := ReadInteger(PROJECT_SECTION, 'MrkOpacity', UpDownMrkOpacity.Position);
    CheckBoxRenEnabled.Checked            := ReadBool(PROJECT_SECTION,    'RenEnabled', CheckBoxRenEnabled.Checked);
    RadioButtonRenSimple.Checked          := ReadBool(PROJECT_SECTION,    'RenSimple', RadioButtonRenSimple.Checked);
    RadioButtonRenAdvanced.Checked        := ReadBool(PROJECT_SECTION,    'RenAdvanced', RadioButtonRenAdvanced.Checked);
    RadioButtonRenCustom.Checked          := ReadBool(PROJECT_SECTION,    'RenCustom', RadioButtonRenCustom.Checked);
    EditRenTemplate.Text                  := ReadString(PROJECT_SECTION,  'RenTemplate', EditRenTemplate.Text);
    CheckBoxShuffle.Checked               := ReadBool(PROJECT_SECTION,    'ShuffleEnabled', CheckBoxShuffle.Checked);
    ComboBoxShuffleSeed.Text              := ShuffleSeedToStr(ReadInteger(PROJECT_SECTION, 'ShuffleSeed', 0));
    CheckBoxTagsSourceEXIF.Checked        := ReadBool(PROJECT_SECTION,    'TagsSourceEXIF', CheckBoxTagsSourceEXIF.Checked);
    CheckBoxTagsSourceTagsFiles.Checked   := ReadBool(PROJECT_SECTION,    'TagsSourceTagsFiles', CheckBoxTagsSourceTagsFiles.Checked);
    CheckBoxTagTitle.Checked              := ReadBool(PROJECT_SECTION,    'TagTitle', CheckBoxTagTitle.Checked);
    CheckBoxTagTimestamp.Checked          := ReadBool(PROJECT_SECTION,    'TagTimestamp', CheckBoxTagTimestamp.Checked);
    CheckBoxTagCopyright.Checked          := ReadBool(PROJECT_SECTION,    'TagCopyright', CheckBoxTagCopyright.Checked);
    EditCopyright.Text                    := ReadString(PROJECT_SECTION,  'Copyright', EditCopyright.Text);
    CheckBoxTagsReportEnabled.Checked     := ReadBool(PROJECT_SECTION,    'TagsReportEnabled', CheckBoxTagsReportEnabled.Checked);
    CheckBoxImageInfosEnabled.Checked     := ReadBool(PROJECT_SECTION,    'ImageInfosEnabled', CheckBoxImageInfosEnabled.Checked);
    CheckBoxDryRun.Checked                := ReadBool(PROJECT_SECTION,    'DryRun', DEFAULT_DRYRUN);

    ActionParamSizes.Execute;
  end;
  FPresentationSettings.Load(Ini);
  FPresentationParamsList.Load(Ini, PRESENTATIONS_GROUP);
end;

const
  SRCMODES :array[boolean] of string = ('Folder', 'Filenames');

procedure TMainDialog.SaveProjectToIni(Ini :TCustomIniFile; SavePathesAsIs :boolean);
var
  Value :integer;

  function HandleFilenameRefs(Filenames :TStrings) :string; overload;
  var
    i :integer;
    Filename :string;
    CurrentDir :string;
    ProjectDir :string;
  begin
    if FDialogSettings.RelPathes.Value and not SavePathesAsIs then begin
      CurrentDir := GetCurrentDir;
      ProjectDir := ExtractFilePath(Ini.Filename);
      for i:=0 to Filenames.Count-1 do begin
        Filename := CreateAbsolutePath(Filenames[i], CurrentDir);
        Filenames[i] := CreateRelativePath(Filename, ProjectDir, true);
      end;
    end;
    result := ReplaceStr(Filenames.Text, #13#10, LINESEP)
  end;

  function HandleFilenameRefs(Edit :TEdit) :string; overload;
  var
    CurrentDir :string;
    ProjectDir :string;
    RelativePath :string;
  begin
    if FDialogSettings.RelPathes.Value and not SavePathesAsIs then begin
      CurrentDir := GetCurrentDir;
      ProjectDir := ExtractFilePath(Ini.Filename);
      RelativePath := CreateRelativePath(CreateAbsolutePath(Edit.Text, CurrentDir), ProjectDir, true);
      //if RelativePath='' then RelativePath := '.\';
      Edit.Text := RelativePath;
    end;
    result := Edit.Text;
  end;

begin
  // Navigate to proper "directory":
  with Ini do begin
    WriteString(COMMON_SECTION, 'Type', PRJTYPE);
    WriteString(COMMON_SECTION, 'Version', PRJVERSION);
    EraseSection(PROJECT_SECTION);
    WriteString(PROJECT_SECTION,  'Description',    FProjectDescription);
    WriteString(PROJECT_SECTION,  'Source',         SRCMODES[ActionSrcFilenames.Checked]);
    WriteString(PROJECT_SECTION,  'SourceFolder',   HandleFilenameRefs(EditSrcFolder));
    WriteString(PROJECT_SECTION,  'SourceMasks',    EditSrcMasks.Text);
    WriteString(PROJECT_SECTION,  'SourceFilenames', HandleFilenameRefs(MemoSrcFilenames.Lines));
//    WriteString(PROJECT_SECTION,  'Sizes',          EditSizes.Text);
//    WriteString(PROJECT_SECTION,  'SizeNames',      ComboBoxSizeNames.Text);
    WriteString(PROJECT_SECTION,  'TargetFolder',   HandleFilenameRefs(EditTargetFolder));
    WriteString(PROJECT_SECTION,  'Interpolation',  INTERPOLATION_NAMES[TInterpolation(ComboBoxInterpolation.ItemIndex)]);
    Value := TProcessor.StrToJPEGQuality(ComboBoxJPEGQuality.Text);
    if Value = DEFAULTJPEGQUALITY then
      WriteString(PROJECT_SECTION, 'JPEGQuality',   JPEGQUALITY_NAMES[0])
    else
      WriteInteger(PROJECT_SECTION, 'JPEGQuality',  Value);
    WriteString(PROJECT_SECTION,  'PNGCompression', PNGCOMPRESSION_NAMES[ComboBoxPNGCompression.ItemIndex]);
    WriteBool(PROJECT_SECTION,    'MrkEnabled',     CheckBoxMrkEnabled.Checked);
    WriteString(PROJECT_SECTION,  'MrkFilename',    HandleFilenameRefs(EditMrkFilename));
    WriteString(PROJECT_SECTION,  'MrkSize',        EditMrkSize.Text);
    WriteString(PROJECT_SECTION,  'MrkX',           EditMrkX.Text);
    WriteString(PROJECT_SECTION,  'MrkY',           EditMrkY.Text);
    WriteString(PROJECT_SECTION,  'MrkOpacity',     EditMrkOpacity.Text);
    WriteBool(PROJECT_SECTION,    'RenEnabled',     CheckBoxRenEnabled.Checked);
    WriteBool(PROJECT_SECTION,    'RenSimple',      RadioButtonRenSimple.Checked);
    WriteBool(PROJECT_SECTION,    'RenAdvanced',    RadioButtonRenAdvanced.Checked);
    WriteBool(PROJECT_SECTION,    'RenCustom',      RadioButtonRenCustom.Checked);
    WriteString(PROJECT_SECTION,  'RenTemplate',    EditRenTemplate.Text);
    WriteBool(PROJECT_SECTION,    'ShuffleEnabled', CheckBoxShuffle.Checked);
    WriteInteger(PROJECT_SECTION, 'ShuffleSeed',    StrToShuffleSeed(ComboBoxShuffleSeed.Text));
    WriteBool(PROJECT_SECTION,    'TagsSourceEXIF', CheckBoxTagsSourceEXIF.Checked);
    WriteBool(PROJECT_SECTION,    'TagsSourceTagsFiles', CheckBoxTagsSourceTagsFiles.Checked);
    WriteBool(PROJECT_SECTION,    'TagTitle',       CheckBoxTagTitle.Checked);
    WriteBool(PROJECT_SECTION,    'TagTimestamp',   CheckBoxTagTimestamp.Checked);
    WriteBool(PROJECT_SECTION,    'TagCopyright',   CheckBoxTagCopyright.Checked);
    WriteString(PROJECT_SECTION,  'Copyright',      EditCopyright.Text);
    WriteBool(PROJECT_SECTION,    'TagsReportEnabled', CheckBoxTagsReportEnabled.Checked);
    WriteBool(PROJECT_SECTION,    'ImageInfosEnabled', CheckBoxImageInfosEnabled.Checked);
    WriteBool(PROJECT_SECTION,    'DryRun',       CheckBoxDryRun.Checked);
  end;
  if FPresentationSettings.Dirty then
    FPresentationSettings.Save(Ini);
  FPresentationParamsList.Save(Ini, PRESENTATIONS_GROUP);
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
    SaveProjectToIni(Ini, True);
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
    SaveProjectToIni(Ini, False);
    FProjectFilename := Filename;
    SetTitle(''''+FProjectFilename+'''');
    ChangeCurrentDir(ExtractFilePath(FProjectFilename));
    Log(Format(SMsgProjectSavedToFmt, [FProjectFilename]), llHint);
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
  if FDialogSettings.WarnDirty.Value and Dirty then begin
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

procedure TMainDialog.ActionNewExecute(Sender: TObject);
begin
  if not CheckSave then Exit;
  InitProject;
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

procedure TMainDialog.ComboBoxSizeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Canvas :TCanvas;
  ImageIndex :integer;
  Size :integer;
begin
  if Index=-1 then Exit;
  Canvas := ComboBoxSize.Canvas;
  Canvas.Brush.Color := IfThen(odSelected in State, clHighlight, clWindow);
  Canvas.FillRect(ARect);
  Size := StrToInt(ComboBoxSize.Items[Index]);
  if Size<=THUMBNAILIMGMAX then
    ImageIndex := 0
  else if Size<=DOCIMGMAX then
    ImageIndex := 1
  else
    ImageIndex := 2;
  ImagesModule.ImageList24x24.Draw(Canvas, 0, ARect.Top, ImageIndex);
  Canvas.TextOut(28, ARect.Top+4, ComboBoxSize.Items[Index]);
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

procedure TMainDialog.ActionHelpScreenshotsExecute(Sender: TObject);
begin
  ActionHelpScreenshots.Visible := false;
  ActionNew.Execute;
  ControlShot.SnapshotFolder := ExtractFilepath(Application.Exename) + '..\hlp\gui\'+TLanguage.Code+'\img';
//  ControlShot.SnapshotFolder := 'C:\TEMP\hlp';
  ToolButtonSrcFilenames.Click;
  with MemoSrcFilenames.Lines do begin
    Clear;
    Add('myphotos\DSC06237.jpg');
    Add('myphotos\DSC06238.jpg');
    Add('myphotos\DSC06227.jpg');
    Add('myphotos\DSC06403.jpg');
  end;
  Snapshot('step-srcimages', PanelSource, -2, -40, 360, 165);
  ToolButtonSizes.Click;
//  EditSizes.Text := '360';
  Snapshot('step-sizes', PanelParams, -2, -40, 360, 95);
  EditTargetFolder.Text := 'mygallery\img{SIZE}';
  Snapshot('step-targetfolder', PanelTarget, -200, -8, 360, 60);
  Snapshot('step-execute', ButtonExecute, 0, 0, 0, 0);
  Snapshot('buttons-project', ToolButtonNew, 0, 0, 4*ToolButtonNew.Width, ToolButtonNew.Height);
//  EditSizes.Text := '120, 800, 1920';
//  Snapshot('sizes-multiple', EditSizes, -6, -24, 200, 32);

  ToolButtonQuality.Click;
  ComboBoxInterpolation.ItemIndex := 7;
  ComboBoxInterpolation.DroppedDown := true;
  Snapshot('quality-interpolation', GroupBoxInterpolationQuality, -8, 0, 300, 270);
  ComboBoxJPEGQuality.ItemIndex := 5;
  ComboBoxJPEGQuality.DroppedDown := true;
  Snapshot('quality-JPEG', GroupBoxJPEGQuality, -8, 0, 270, 210);
  ComboBoxPNGCompression.ItemIndex := 2;
  ComboBoxPNGCompression.DroppedDown := true;
  Snapshot('quality-PNG', GroupBoxPNGCompression, -8, 0, 270, 130);
  ComboBoxPNGCompression.DroppedDown := false;

  ToolButtonTagging.Click;
  Snapshot('tagging-save', GroupBoxTaggingSave, -8, 0, 0, 0);
  Snapshot('tagging-load', GroupBoxTaggingLoad, -8, 0, 0, 0);
  Snapshot('tagging-EXIF', GroupBoxTaggingEXIF, -8, 0, 0, 0);

  ActionHelpScreenshots.Visible := true;
  //if not Assigned(MrkEditDialog) then
  //Snapshot('buttons-watermark', MrkEditDialog.ToolBar, 0, 0, MrkEditDialog.ToolBar.Width, MrkEditDialog.ToolBar.Height);
end;

procedure TMainDialog.ActionPresentationExecute(Sender: TObject);
begin
  if PresentationDialog.Execute(FPresentationSettings, FPresentationParamsList) then begin
    if FPresentationSettings.Dirty or FPresentationParamsList.Dirty then
      Dirty := true;
  end;
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
  if Pos('{SIZE}', s)>0 then
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
  EditTargetFolder.SelText := '{SIZE}';
end;

procedure TMainDialog.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log(SLogErrorPrefix+E.Message, llError);
end;

procedure TMainDialog.ListBoxSizesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  Canvas :TCanvas;
  h :integer;
  tr :TRect;
const
  trSize:TTextStyle=(Alignment:taRightJustify;Layout:tlCenter;SingleLine:true;Clipping:true;ExpandTabs:false;
    ShowPrefix:false;Wordbreak:false;Opaque:false;SystemFont:false;RightToLeft:false;EndEllipsis:true);
  trName:TTextStyle=(Alignment:taLeftJustify;Layout:tlCenter;SingleLine:true;Clipping:true;ExpandTabs:false;
    ShowPrefix:false;Wordbreak:false;Opaque:false;SystemFont:false;RightToLeft:false;EndEllipsis:true);
begin
  h := ListBoxSizes.ItemHeight;
  Canvas := ListBoxSizes.Canvas;
  Canvas.Brush.Color := IfThen(odSelected in State, clHighlight, clWindow);
  Canvas.FillRect(ARect);
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Left+2, ARect.Top+2, ifThen(FSizeInfos[Index].Enabled, 4, 3));
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Left+2+h, ARect.Top+2, Index);
  Canvas.Font.Size := 10;
  Canvas.Font.Style := [fsbold];
  tr := ARect; tr.Right := tr.Left + 100;
  Canvas.TextRect(tr, 58, 2, IntToStr(FSizeInfos[Index].Size), trSize);
  Canvas.Font.Style := [];
  tr.Left := 104;
  tr.Right := ARect.Right;
  Canvas.TextRect(tr, 104, 2, FSizeInfos[Index].Name, trName);
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Right-h-2, ARect.Top+2, 5);
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
  ActionSave.ImageIndex := IfThen(Dirty, IMGIDX_DIRTY, IMGIDX_SAVE);
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
const
  IMGIDXS :array[boolean] of integer = (23, 22);
begin
  ImageStep1.ImageIndex := IMGIDXS[FRequiredSteps[1]];
  ImageStep2.ImageIndex := IMGIDXS[FRequiredSteps[2]];
  ImageStep3.ImageIndex := IMGIDXS[FRequiredSteps[3]];
  for Required in FRequiredSteps do
    if Required then begin
      ActionExecute.Enabled := false;
      Exit;
    end;
  ActionExecute.Enabled := true;
end;

procedure TMainDialog.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  MemoSrcFilenames.Lines.AddStrings(Filenames);
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
  //SizeDict :TSizeDict;
  //Sizes :TSizes;
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
      //if TrySizesStrToSizes(EditSizes.Text, Sizes) then begin
      //  EditSizes.Text := SizesToSizesStr(Sizes);
      //  RequiredSteps[2] := Length(Sizes)=0;
      //end else begin
      //  RequiredSteps[2] := true;
      //  if EditSizes.Focused then
      //    Warning(SErrInvalidSizes);
      //end;
      //SizeDict := TSizeDict.Create;
      //try
      //  for i:=0 to High(Sizes) do
      //    SizeDict.Add(Sizes[i], i);
      //  with ToolBarSizeButtons do for i:=0 to ButtonCount-1 do
      //    Buttons[i].Down := SizeDict.ContainsKey(Buttons[i].Tag);
      //finally
      //  SizeDict.Free;
      //end;
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
  ImgRect :TRect;
  MarkRect :TRect;
  Bmp :TBGRABitmap;
  BorderColor :TBGRAPixel;
  FillColor :TBGRAPixel;
  Transparency :Byte;
begin
  if CalcMarkRect(MarkRect) then begin
    ImgRect := TRect.Create(0, 0, MarkRect.Width, MarkRect.Height);
    Transparency := round(255*StrToFloat(EditMrkOpacity.Text)/100.0);
    BorderColor.FromColor(clBlack);
    FillColor.FromColor(clBlue, Transparency);
    Bmp := TBGRABitmap.Create(MarkRect.Width, MarkRect.Height);
    try
      Bmp.Rectangle(ImgRect, BorderColor, FillColor, dmLinearBlend);
      Bmp.Draw(PaintBoxMrkPreview.Canvas, MarkRect, False);
    except
    end;
    Bmp.Free;
  end;
end;

procedure TMainDialog.ProgressBarPaint(Sender: TObject);
var
  r :TRect;
begin
  r := ProgressBar.ClientRect;
  r.Right := round(r.Left + FProgress*(r.Width));
  with ProgressBar.Canvas do begin
    Brush.Color := STYLECOLOR_DARK;
    FillRect(r);
  end;
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

procedure TMainDialog.ActionSaveAsExecute(Sender: TObject);
begin
  SaveAsDialog.Filename := FProjectFilename;
  if SaveAsDialog.Execute then
    SaveProjectToFile(SaveAsDialog.Filename);
end;

procedure TMainDialog.ActionAboutExecute(Sender: TObject);
begin
  TAboutDialog.Execute(IMGRESGUICPR, SCptProcessor + ' ' +IMGRESVER, 'LICENSE_'+TLanguage.Code);
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
  Application.ProcessMessages;
  if FCancelled then
    (Sender as TProcessor).Cancel;
end;

procedure TMainDialog.ActionExecuteExecute(Sender: TObject);
var
  //Sizes :TIntegerDynArray;
  //SizeNames :TStringArray;
  x :single;
  p :TPos;
  TargetFolder :string;
  IntValue :integer;
  SourceFilenames :TStringList;
  TagsSources :TTagsSources;
  TagsReports :TTagsReports;
  TagKeys :TStringArray;
  Processor :TProcessor;
  WatermarkParams :TProcessor.TWatermarkParams;

  procedure EnableActions(Value :boolean);
  begin
    ActionNew.Enabled := Value;
    ActionOpen.Enabled := Value;
    ActionSave.Enabled := Value;
    ActionSaveAs.Enabled := Value;
    ActionSettings.Enabled := Value;
    ActionEditWatermark.Enabled := Value;
    ActionPresentation.Enabled := Value;
    PanelSource.Enabled := Value;
    PanelParams.Enabled := Value;
    PanelTarget.Enabled := Value;
  end;

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
    EnableActions(False);
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
          // Source Filenames
          if (MemoSrcFilenames.Text='') then
            raise Exception.Create(SErrMissingSourceFilenames);
          SourceFilenames.Assign(MemoSrcFilenames.Lines);
        end else begin
          // Source Folder
          if (EditSrcFolder.Text='') or not DirectoryExists(EditSrcFolder.Text) then
              raise Exception.Create(SErrMissingSourceFolder);
          FindAllFiles(SourceFilenames, EditSrcFolder.Text, EditSrcMasks.Text, false);
        end;

        // Destination folder
        if (EditTargetFolder.Text='') then
          raise Exception.Create(SErrMissingDestinationFolder);

        // Sizes
        //Sizes := nil;
        //if EditSizes.Text = SCptSizesDefault then begin
        //  SetLength(Sizes, 1);
        //  Sizes[0] := DEFAULTSIZE;
        //end else if not TrySizesStrToSizes(EditSizes.Text, Sizes) then
        //  raise Exception.Create(SErrInvalidSizes);

        //// SizeNames
        //SizeNames := StrToStringArray(ComboBoxSizeNames.Text);

        // Quality
        Processor := TProcessor.Create;
        if not TProcessor.TryStrToJPEGQuality(ComboBoxJPEGQuality.Text, IntValue) then
          raise Exception.Create(SErrInvalidJpgQuality);
        Processor.JPEGQuality := IntValue;
        Processor.PngCompression := ComboBoxPNGCompression.ItemIndex;
        Processor.Interpolation := TInterpolation(ComboBoxInterpolation.ItemIndex);

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
        WatermarkParams.Filename := EditMrkFilename.Text;

        if not TryStrToFloat(EditMrkSize.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkSize);
        WatermarkParams.Size := x;

        if not TryStrToFloat(EditMrkX.Text, p.x) or (p.x<0.0) or (p.x>100.0) then
          raise Exception.Create(SErrInvalidMrkXBrd);
        if not TryStrToFloat(EditMrkY.Text, p.y) or (p.y<0.0) or (p.y>100.0) then
          raise Exception.Create(SErrInvalidMrkYBrd);

        WatermarkParams.X := StrToFloat(EditMrkX.Text);
        WatermarkParams.Y := StrToFloat(EditMrkY.Text);

        if not TryStrToFloat(EditMrkOpacity.Text, x) or (x<0.0) or (x>100.0) then
          raise Exception.Create(SErrInvalidMrkOpacity);
        WatermarkParams.Opacity := x;

        Processor.WatermarkParams := WatermarkParams;

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
        if CheckBoxImageInfosEnabled.Checked then include(TagsReports, trImgTags);
        Processor.TagsReports := TagsReports;

        // DryRun flag
        Processor.DryRun := CheckBoxDryRun.Checked;

        // stop, if %SIZE% placeholder is not contained either in
        // TargetFolder nor in FileTemplate
        TargetFolder := EditTargetFolder.Text;
        //if (Length(Sizes)>1) and (Pos('{SIZE}', TargetFolder)+Pos('{SIZENAME}', TargetFolder)=0)
        // and not (Processor.RenEnabled and (Pos('{SIZE}', Processor.TargetFiletemplate)+Pos('{SIZENAME}', Processor.TargetFiletemplate)>0)) then
        //  raise Exception.Create(SErrEnterPlaceholder);
        // Hook the processor
        Processor.OnPrint := OnPrint;
        Processor.OnProgress := OnProgress;

        //Processor.Sizes := SizesToSizesStr(Sizes);
        //Processor.SizeNames := SizeNames;
        Processor.SourceFilenames := SourceFilenames;
        Processor.TargetFolder := TargetFolder;
        Processor.ThreadCount := FProcessingSettings.ThreadsUsed.Value;
        Processor.StopOnError := FProcessingSettings.StopOnError.Value;
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
      EnableActions(True);
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

//procedure test;
//var
//  dir1, dir2 :string;
//begin
//  dir1 := '.\';
//  SetCurrentDir('D:\mf\Dev\Lazarus\ImageResize\tst');
//  dir2 := CleanAndExpandDirectory(dir1);
//
//
//end;
//
//initialization
//begin
//  test;
//end;
//
end.

