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
  Windows, LCLTranslator, Classes, Types, SysUtils, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ComCtrls, ActnList, ExtCtrls, imgres, AboutDlg, IniFiles,
  StrUtils, LMessages, LCLIntf, Buttons, ImgList, LCLType, LazHelpHTML, Menus,
  BGRABitmap, BGRABitmapTypes, BGRASpeedButton, BGRAGraphicControl,
  RichMemo, MrkEditDlg, WinDirs, UpdateUtils, Settings,
  AppSettings, Logging, LoggingRichMemo, StringArrays, Presentations,
  PresentationDlg, TagIds, LazFileUtils, Translations, Language,
  ControlShot, IniFilesHelper, ImagesMod, Templates;

const
  LM_RUN                = LM_USER + 1;

  GUIVER_APP            = 'ImageResize';
  GUIVER_VERSION        = '4.2';
  GUIVER_DATE           = '2024-10-27';

  GUIVER                :TVersionManifest = (App: GUIVER_APP; Version: GUIVER_VERSION; Date: GUIVER_DATE; Hint: '');

  //APPWEBURL             = 'www.atomek.de/imageresize/index.html';
  APPDOWNLOADURL        = 'www.atomek.de/imageresize/download/InstallImageResize.zip';
  //APPGITHUBURL          = 'https://github.com/Atomek61/ImageResize';
  GUIVERURL             = 'https://www.atomek.de/imageresize/download/version.manifest';

  IMGRESGUICPR          =  GUIVER_APP+' '+GUIVER_VERSION+' © 2024 Jan Schirrmacher, www.atomek.de';

  PRJTYPE               = 'IRS';
  PRJVERSION200         = '200';
  PRJVERSION210         = '210';
  PRJVERSION300         = '300';
  PRJVERSION400         = '400';
  PRJVERSION            = '420';

  SETTYPE               = 'IST';
  SETVERSION            = '100';

  LASTPROJECT_FILENAME  = 'lastproject.irs';
  SETTINGS_FILENAME     = 'settings.ini';

  COMMON_SECTION        = 'Common';
  PROJECT_SECTION       = 'Project';
  PROCESSING_SECTION    = 'Processor';
  MAINDIALOG_SECTION    = 'MainDialog';
  PRESDIALOG_SECTION    = 'PresentationDialog';
  MAINDIALOG_FILEHISTORY= 'FileHistory';

  FILEHISTORY_NUMENTRIES= 8;

  RENSIMPLETEMPLATE     = 'img$(INDEX.ifmt(1)).$(FILEEXT)';
  RENADVANCEDTEMPLATE   = 'img$(INDEX.ifmt(1))_$(SIZE).$(FILEEXT)';
  DEFAULT_SRCMASK       = '*.jpg;*.png';

  SIZEBTNHINTFMT        = '%s - %dpx';

  MRKRECTRATIO          = 3.0;

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
  SUrlWebHelp = 'http://www.atomek.de/imageresize/hlp42/gui/en';
  SLocDirHelp = 'hlp\gui\en';

type

  { TMainDialog }

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TMainDialog = class(TForm)
    ActionParamPresentation: TAction;
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
    ButtonAddSize: TBitBtn;
    ButtonReplaceSize: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonDeleteSize: TBitBtn;
    ComboBoxSharpen: TComboBox;
    ComboBoxSize: TComboBox;
    ComboBoxSizeName: TComboBox;
    GroupBoxSharpen: TGroupBox;
    ImageStep1: TImage;
    ImageStep2: TImage;
    ImageStep3: TImage;
    Label19: TLabel;
    Label2: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    LabelCurrentDir: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LabelRenTemplate: TLabel;
    ListBoxSizesPalette: TListBox;
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
    PopupMenuFileHistory: TPopupMenu;
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
    ToolButtonPresentation: TToolButton;
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
    procedure ButtonAddSizeClick(Sender: TObject);
    procedure ButtonBrowseTargetFolderClick(Sender: TObject);
    procedure ButtonBrowseMrkFilenameClick(Sender: TObject);
    procedure ButtonBrowseSrcFilesClick(Sender: TObject);
    procedure ButtonBrowseSrcFolderClick(Sender: TObject);
    procedure ButtonClearCopyrightClick(Sender: TObject);
    procedure ButtonClearSizesClick(Sender: TObject);
    procedure ButtonClearSrcFilesClick(Sender: TObject);
    procedure ButtonDeleteSizeClick(Sender: TObject);
    procedure ButtonInsertCopyrightClick(Sender: TObject);
    procedure ButtonInsertSIZEClick(Sender: TObject);
    procedure ActionParamExecute(Sender :TObject);
    procedure ButtonReplaceSizeClick(Sender: TObject);
    procedure ComboBoxSizeDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBoxSizeMeasureItem(Control: TWinControl; Index: Integer;
      var AHeight: Integer);
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
    procedure ListBoxSizesClick(Sender: TObject);
    procedure ListBoxSizesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBoxSizesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListBoxSizesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ListBoxSizesPaletteClick(Sender: TObject);
    procedure ListBoxSizesPaletteDblClick(Sender: TObject);
    procedure ListBoxSizesPaletteDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ListBoxSizesPaletteMeasureItem(Control: TWinControl;
      Index: Integer; var AHeight: Integer);
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
    FProjectDir :string; // Only valid after loading from project file
    FProjectFilename :string;
    FProjectDescription :string; // From project file
    FPresentationSettings :TPresentationSettings;
    FPresentationParamsList :TSettingsList;
    FSizeInfos :TSizeInfos;
    FDefSizes :TSizes;
    FScreenSize :integer;
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
//    FProjectDirectory :string;
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
    procedure AddToFileHistory(const Filename :string);
    procedure HistoryFileClick(Sender :TObject);
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
    procedure OnSizeInfosChanged(Sender :TSizeInfos; Change :TSizeInfos.TChange; Index :integer);
  public
    property RequiredSteps[Index :integer] :boolean read GetRequiredSteps write SetRequiredSteps;
    property Dirty :boolean read FIsDirty write SetDirty;
  end;

var
  MainDialog: TMainDialog;

implementation

uses
  math, helpintfs, FileUtil, SettingsDlg;

resourcestring
  SCptProcessor                 = 'Processor';
  SMsgQuerySave                 = 'The project has been modified.'+#10+#10+'Do you want to save the changes?';
  SCptQuerySave                 = 'Project unsaved';
  SMsgProjectDescriptionFmt     = 'Description: "%s"';
  SCptUnnamed                   = '<unnamed>';
  SErrInvalidShuffleSeed        = 'Invalid Shuffle Seed';
  SCptTitlePrefix               = 'ImageResize - ';
  SCptLastProject               = '<last project>';
  SCptCancel                    = 'Cancel';
  SCptExecute                   = 'Execute';
  SCptExecuteA                  = 'E&xecute';
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
  SErrInvalidSharpen            = 'Invalid sharpen value';
  SErrInvalidMrkSize            = 'Invalid watermark size';
  SErrInvalidMrkXBrd            = 'Invalid watermark x border';
  SErrInvalidMrkYBrd            = 'Invalid watermark y border';
  SErrInvalidMrkOpacity         = 'Invalid watermark opacity';
  SErrEnterPlaceholder          = 'Enter placeholder $(SIZE) or $(SIZENAME) to either the target folder or the file template';
  SErrAtFmt                     = 'Error at %.0f%% - %s';
  SErrCancelledAtFmt            = 'Cancelled at %.0f%%';

const
  TRSIZE:TTextStyle=(Alignment:taRightJustify;Layout:tlCenter;SingleLine:true;Clipping:true;ExpandTabs:false;
    ShowPrefix:false;Wordbreak:false;Opaque:false;SystemFont:false;RightToLeft:false;EndEllipsis:true);

  TRNAME:TTextStyle=(Alignment:taLeftJustify;Layout:tlCenter;SingleLine:true;Clipping:true;ExpandTabs:false;
    ShowPrefix:false;Wordbreak:false;Opaque:false;SystemFont:false;RightToLeft:false;EndEllipsis:true);

  TRDEFSIZE:TTextStyle=(Alignment:taCenter;Layout:tlCenter;SingleLine:true;Clipping:true;ExpandTabs:false;
    ShowPrefix:false;Wordbreak:false;Opaque:false;SystemFont:false;RightToLeft:false;EndEllipsis:true);

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

procedure TMainDialog.OnSizeInfosChanged(Sender: TSizeInfos; Change: TSizeInfos.TChange; Index: integer);
var
  i :integer;
begin
  case Change of
  lcChanged:
    ListBoxSizes.Invalidate;
  lcInsert:
    begin
      ListBoxSizes.Items.Insert(Index, IntToStr(FSizeInfos[Index].Size));
      ListBoxSizes.ItemIndex := Index;
    end;
  lcDelete:
    begin
      ListBoxSizes.Items.Delete(Index);
      if Index>=ListBoxSizes.Items.Count then
        ListBoxSizes.ItemIndex := ListBoxSizes.Items.Count-1
      else
        ListBoxSizes.ItemIndex := Index;
    end;
  lcInit:
    begin
      ListBoxSizes.Items.Clear;
      for i:=0 to FSizeInfos.Count-1 do
        ListBoxSizes.Items.Add(IntToStr(FSizeInfos[i].Size));
      ComboBoxSize.ItemIndex := -1;
      ComboBoxSizeName.Text := TSizeInfos.SIZENAMES[snAuto];
    end;
  end;
  Dirty := True;
  UpdateRequiredStep(2);
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

  FSizeInfos := TSizeInfos.Create;
  FSizeInfos.OnChanged := OnSizeInfosChanged;

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

  SetLength(FDefSizes, Length(TSizeInfos.DEFAULT_SIZES));
  for i:=0 to High(TSizeInfos.DEFAULT_SIZES) do
    FDefSizes[i] := TSizeInfos.DEFAULT_SIZES[i];
  FScreenSize := IfThen(Screen.Width>Screen.Height, Screen.Width, Screen.Height);
  for i:=0 to High(FDefSizes) do
    if FScreenSize=FDefSizes[i] then
      break
    else if FScreenSize<FDefSizes[i] then begin
      SetLength(FDefSizes, Length(FDefSizes)+1);
      Move(FDefSizes[i], FDefSizes[i+1], (Length(FDefSizes)-i)*sizeof(integer));
      FDefSizes[i] := FScreenSize;
      break
    end else if i=High(FDefSizes) then begin
      SetLength(FDefSizes, Length(FDefSizes)+1);
      FDefSizes[i+1] := FScreenSize;
    end;

  for i in FDefSizes do
    ComboBoxSize.Items.Add(IntToStr(i));

  for i in FDefSizes do
    ListBoxSizesPalette.Items.Add(IntToStr(i));

  for Interpolation in TInterpolation do
    ComboBoxInterpolation.Items.Add(INTERPOLATION_STRINGS[Interpolation]);

  ComboBoxJPEGQuality.Items.Clear;
  for Item in JPEGQUALITY_STRINGS do
    ComboBoxJPEGQuality.Items.Add(Item);

  ComboBoxPNGCompression.Items.Clear;
  for Item in PNGCOMPRESSION_STRINGS do
    ComboBoxPNGCompression.Items.Add(Item);

  ComboBoxSharpen.Items.Clear;
  for Item in SHARPEN_STRINGS do
    ComboBoxSharpen.Items.Add(Item);

  ComboBoxShuffleSeed.Items[0] := SCptRandomSeed;

end;

procedure TMainDialog.FormShow(Sender: TObject);
var
  Filename :string;
  LocHelpDir :string;
begin

  // Evaluate commandline parameters
  FAutoExit := IsSwitch('X', 'AUTOEXIT');
  Filename := GetNonSwitch;
  if IsSwitch('A', 'AUTOSTART') then
    PostMessage(Handle, LM_RUN, 0, 0);

  LoadSettings;
  ActionNew.Execute;

  if Filename<>'' then begin
    LoadProjectFromFile(Filename);
  end else if FDialogSettings.AutoSave.Value then begin
    LoadLastProject;
  end else
    LabelCurrentDir.Caption := GetCurrentDir;

  RequiredStepsUpdate;

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
  FSizeInfos.Free;
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
    FSizeInfos.Clear;
    ComboBoxSize.ItemIndex              := -1;
    ComboBoxSizeName.ItemIndex          := 0;
    EditTargetFolder.Text               := '';
    ComboBoxJPEGQuality.ItemIndex       := 0;
    ComboBoxPNGCompression.ItemIndex    := 0;
    ComboBoxSharpen.ItemIndex           := 0;
    ComboBoxInterpolation.ItemIndex     := 0;
    EditMrkFilename.Text                := '';
    with ImgResizer.WatermarkParams do begin
      UpDownMrkSize.Position            := round(Size);
      UpDownMrkX.Position               := round(X);
      UpDownMrkY.Position               := round(Y);
      UpDownMrkOpacity.Position           := round(Opacity);
    end;
    FIsSave                             := false;
    CheckBoxRenEnabled.Checked          := ImgResizer.Rename;
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
    CheckBoxDryRun.Checked              := false;

    ActionSrcFilenames.Execute;
    ActionParamSizes.Execute;

    FPresentationSettings.SetDefaults;
    FPresentationParamsList.Clear;

    RequiredStepsUpdate;
    SetTitle(SCptUnnamed);
//    FProjectDirectory := '';
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
  i :integer;
  Item :string;
  MenuItem :TMenuItem;
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
    //Path := Ini.ReadString(MAINDIALOG_SECTION, 'CurrentDirectory', GetCurrentDir);
    //if DirectoryExists(Path) then
    //  ChangeCurrentDir(Path);
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

    i := 1;
    while true do begin
      Item := Ini.ReadString(MAINDIALOG_FILEHISTORY, IntToStr(i), '');
      if Item='' then break;
      MenuItem := TMenuItem.Create(self);
      MenuItem.OnClick := HistoryFileClick;
//      MenuItem.Caption := IntToStr(i)+'. '+Item;
      MenuItem.Caption := Item;
      PopupMenuFileHistory.Items.Add(MenuItem);
      inc(i);
    end;

    FProcessingSettings.Load(Ini, PROCESSING_SECTION);
  finally
    Free;
  end;
end;

procedure TMainDialog.SaveSettings;
var
  Ini :TCustomIniFile;
  Item :string;
  i :integer;
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

    for i:=0 to PopupMenuFileHistory.Items.Count-1 do begin
      Item := PopupMenuFileHistory.Items[i].Caption;
//      Item := Copy(Item, 4, Length(Item)-3);
      Ini.WriteString(MAINDIALOG_FILEHISTORY, IntToStr(i+1), Item);
    end;

    FProcessingSettings.Save(Ini, PROCESSING_SECTION);
  finally
    Free;
  end;
end;

procedure TMainDialog.AddToFileHistory(const Filename: string);
var
  Item :string;
  i :integer;
  NewItem :TMenuItem;

  procedure MoveDown(i0, i1 :integer);
  var
    i :integer;
  begin
    for i := i1 downto i0+1 do
      PopupMenuFileHistory.Items[i].Caption := PopupMenuFileHistory.Items[i-1].Caption;
  end;

begin

  // Check, if already exists
  for i:=0 to PopupMenuFileHistory.Items.Count-1 do begin
    Item := PopupMenuFileHistory.Items[i].Caption;
    //Item := Copy(Item, 4, Length(Item)-3);
    if SameText(Item, Filename) then begin
      MoveDown(0, i);
      // Exists, put on first place
      PopupMenuFileHistory.Items[0].Caption := Filename;
      Exit;
    end;
  end;
  if PopupMenuFileHistory.Items.Count < FILEHISTORY_NUMENTRIES then begin
    NewItem := TMenuItem.Create(self);
    NewItem.OnClick := HistoryFileClick;
    PopupMenuFileHistory.Items.Add(NewItem);
  end;
  MoveDown(0, PopupMenuFileHistory.Items.Count-1);
  PopupMenuFileHistory.Items[0].Caption := Filename;
end;

procedure TMainDialog.HistoryFileClick(Sender: TObject);
var
  Item :string;
begin
  if not CheckSave then Exit;
  Item := (Sender as TMenuItem).Caption;
  LoadProjectFromFile(Item);
end;

function TMainDialog.LoadProjectFromIni(Ini :TCustomIniFile) :boolean;
var
  IniVer :string;
  IniTyp :string;
  Value :integer;
  Str :string;
begin
  with Ini do begin
    IniTyp := ReadString(COMMON_SECTION, 'Type', 'unknown');
    result := IniTyp=PRJTYPE;
    if not result then begin
       Log(SLogCantLoadProject, llWarning);
       Exit;
    end;
    IniVer := ReadString(COMMON_SECTION, 'Version', '000');
    result := (IniVer=PRJVERSION) or (IniVer>=PRJVERSION300);
    if not result then begin
      Log(Format(SLogWarningProjectVersionFmt, [IniVer, PRJVERSION]), llWarning);
      Exit;
    end;

    InitProject;
    FProjectDir := ReadString(COMMON_SECTION, 'CurrentDir', GetCurrentDir);
    FProjectDescription := ReadLang(PROJECT_SECTION,  'Description', '', TLanguage.Code);
    if SameText(ReadString(PROJECT_SECTION, 'Source', 'Filenames'), 'Filenames') then
      ActionSrcFilenames.Execute
    else
      ActionSrcFolder.Execute;
    EditSrcFolder.Text                    := ReadString(PROJECT_SECTION,  'SourceFolder', EditSrcFolder.Text);
    EditSrcMasks.Text                     := ReadString(PROJECT_SECTION,  'SourceMasks', EditSrcMasks.Text);
    MemoSrcFilenames.Text                 := ReplaceStr(ReadString(PROJECT_SECTION, 'SourceFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, '|')), '|', #13#10);
    Str                                   := ReadString(PROJECT_SECTION,  'Sizes', '<missing>');
    if Str<>'<missing>' then begin
      if not FSizeInfos.TrySetSizeString(Str) then
        Log(SErrInvalidSizes, llWarning);
    end else
      if not FSizeInfos.TrySetString(ReadString(PROJECT_SECTION,  'SizeNames', '')) then
        Log(SErrInvalidSizes, llWarning);

    EditTargetFolder.Text                 := DOSDELIMITERS.UpgradeFrom(ReadString(PROJECT_SECTION,  'TargetFolder', EditTargetFolder.Text), PERCENTDELIMITERS);
    RequiredStepsUpdate;
    ComboBoxInterpolation.ItemIndex       := integer(TProcessor.NameToInterpolation(ReadString(PROJECT_SECTION,  'Interpolation', INTERPOLATION_NAMES[DEFAULT_INTERPOLATION])));
    Value := ReadInteger(PROJECT_SECTION,  'JPEGQuality', TProcessor.StrToJPEGQuality(ComboBoxJPEGQuality.Text));
    if Value = DEFAULTJPEGQUALITY then ComboBoxJPEGQuality.ItemIndex := 0 else ComboBoxJPEGQuality.Text := IntToStr(Value);
    ComboBoxPNGCompression.ItemIndex      := TProcessor.NameToPNGCompression(ReadString(PROJECT_SECTION,  'PNGCompression', PNGCOMPRESSION_NAMES[ComboBoxPNGCompression.ItemIndex]));
    ComboBoxSharpen.Text                  := TProcessor.SharpenToStr(TProcessor.NameToSharpen(ReadString(PROJECT_SECTION, 'Sharpen', ComboBoxSharpen.Text)));
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
    EditRenTemplate.Text                  := DOSDELIMITERS.UpgradeFrom(ReadString(PROJECT_SECTION,  'RenTemplate', EditRenTemplate.Text), PERCENTDELIMITERS);
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
    result := ReplaceStr(Filenames.Text, #13#10, '|')
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
      Edit.Text := RelativePath;
    end;
    result := Edit.Text;
  end;

begin
  // Navigate to proper "directory":
  with Ini do begin
    WriteString(COMMON_SECTION, 'Type', PRJTYPE);
    WriteString(COMMON_SECTION, 'Version', PRJVERSION);
    WriteString(COMMON_SECTION, 'CurrentDir', GetCurrentDir);
    EraseSection(PROJECT_SECTION);
    WriteString(PROJECT_SECTION,  'Description',    FProjectDescription);
    WriteString(PROJECT_SECTION,  'Source',         SRCMODES[ActionSrcFilenames.Checked]);
    WriteString(PROJECT_SECTION,  'SourceFolder',   HandleFilenameRefs(EditSrcFolder));
    WriteString(PROJECT_SECTION,  'SourceMasks',    EditSrcMasks.Text);
    WriteString(PROJECT_SECTION,  'SourceFilenames', HandleFilenameRefs(MemoSrcFilenames.Lines));
    WriteString(PROJECT_SECTION,  'SizeNames',      FSizeInfos.GetString);
    WriteString(PROJECT_SECTION,  'TargetFolder',   HandleFilenameRefs(EditTargetFolder));
    WriteString(PROJECT_SECTION,  'Interpolation',  INTERPOLATION_NAMES[TInterpolation(ComboBoxInterpolation.ItemIndex)]);
    Value := TProcessor.StrToJPEGQuality(ComboBoxJPEGQuality.Text);
    if Value = DEFAULTJPEGQUALITY then
      WriteString(PROJECT_SECTION, 'JPEGQuality',   JPEGQUALITY_NAMES[0])
    else
      WriteInteger(PROJECT_SECTION, 'JPEGQuality',  Value);
    WriteString(PROJECT_SECTION,  'PNGCompression', PNGCOMPRESSION_NAMES[ComboBoxPNGCompression.ItemIndex]);
    WriteString(PROJECT_SECTION,  'Sharpen',        TProcessor.SharpenToName(TProcessor.StrToSharpen(ComboBoxSharpen.Text)));
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
    ChangeCurrentDir(FProjectDir);
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
      AddToFileHistory(Filename);
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
    AddToFileHistory(Filename);
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

procedure TMainDialog.ButtonReplaceSizeClick(Sender: TObject);
begin
  if ListBoxSizes.ItemIndex<>-1 then
    FSizeInfos.Replace(ListBoxSizes.ItemIndex, True, StrToInt(ComboBoxSize.Text), ComboBoxSizeName.Text)
  else
    ButtonAddSizeClick(nil);
end;

procedure TMainDialog.ComboBoxSizeDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Canvas :TCanvas;
  tr, fr :TRect;
  Size :integer;
begin
  Size := FDefSizes[Index];
  Canvas := ComboBoxSize.Canvas;

  // Background
  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := TSizeInfos.SIZENAMECOLORS[TSizeInfos.SizeToSizename(Size)];
  Canvas.FillRect(ARect);

  // Size
  with ARect do tr := Rect(Left, Top, Left+64, Bottom);

  // Screen Marker
  if FScreenSize=Size then with ARect do begin
    fr := tr; fr.right += 4; fr.Inflate(-1, -2);
    Canvas.Pen.Color := TSizeInfos.SCREENSIZECOLOR;
    Canvas.Frame(fr);
  end;

  // Text
  Canvas.Font.Style := [fsbold];
  Canvas.TextRect(tr, tr.Left, tr.Top+2, ComboBoxSize.Items[Index], TRSIZE);
  Canvas.Brush.Color := clBlue;

  // Size Icon
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Right-26, ARect.Top, integer(TSizeInfos.SizeToSizename(Size)));

end;

procedure TMainDialog.ComboBoxSizeMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  if Index = -1 then
    AHeight := 15
  else
    AHeight := 24;
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
  FSizeInfos.Clear;
  FSizeInfos.Add(True, 360, '<auto>');
  Snapshot('step-sizes', PanelParams, -2, -40, 360, 95);
  EditTargetFolder.Text := 'mygallery\img$(SIZE)';
  Snapshot('step-targetfolder', PanelTarget, -190, -8, 360, 60);
  Snapshot('step-execute', ButtonExecute, 0, 0, 0, 0);
  Snapshot('buttons-project', ToolButtonNew, 0, 0, 4*ToolButtonNew.Width, ToolButtonNew.Height);
  FSizeInfos.Clear;
  FSizeInfos.Add(True, 120, '<auto>');
  FSizeInfos.Add(False, 800, '<auto>');
  FSizeInfos.Add(True, 1920, '<auto>');
  Snapshot('sizes-multiple', ListBoxSizes, -6, -24, 240, 90);

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
  ComboBoxSharpen.ItemIndex := 2;
  ComboBoxSharpen.DroppedDown := true;
  Snapshot('quality-sharpen', GroupBoxSharpen, -8, 0, 270, 130);
  ComboBoxSharpen.DroppedDown := false;

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

procedure TMainDialog.ButtonAddSizeClick(Sender: TObject);
var
  Size :integer;
begin
  Size := StrToInt(ComboBoxSize.Text);
  FSizeInfos.Add(True, Size, TSizeInfos.SizeNameToStr(ComboBoxSizeName.Text, Size));
end;

procedure TMainDialog.ButtonBrowseTargetFolderClick(Sender: TObject);
var
  s :string;
  f :string;
begin
  s := EditTargetFolder.Text;
  f := '';
  if TEngine.ContainsOneOf(s, ['SIZE', 'SIZENAME'], DOSDELIMITERS) then
//  if (Pos('$(SIZE)', s)>0) or (Pos('$(SIZE)', s)>0) then
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
  FSizeInfos.Clear;
end;

procedure TMainDialog.ButtonClearSrcFilesClick(Sender: TObject);
begin
  MemoSrcFilenames.Lines.Clear;
end;

procedure TMainDialog.ButtonDeleteSizeClick(Sender: TObject);
begin
  if ListBoxSizes.ItemIndex<>-1 then
    FSizeInfos.Delete(ListBoxSizes.ItemIndex);
end;

procedure TMainDialog.ButtonInsertCopyrightClick(Sender: TObject);
begin
  EditCopyright.SelText := '© ';
end;

procedure TMainDialog.ButtonInsertSIZEClick(Sender: TObject);
begin
  EditTargetFolder.SelText := '$(SIZE)';
end;

procedure TMainDialog.ApplicationProperties1Exception(Sender: TObject;
  E: Exception);
begin
  Log(SLogErrorPrefix+E.Message, llError);
end;

procedure TMainDialog.ListBoxSizesClick(Sender: TObject);
begin
  if ListBoxSizes.ItemIndex>=0 then begin
    with FSizeInfos[ListBoxSizes.ItemIndex] do begin
      ComboBoxSize.Text := IntToStr(Size);
      ComboBoxSizeName.Text := FSizeInfos.StrToSizeName(Name);
    end;
  end;
end;

procedure TMainDialog.ListBoxSizesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i :integer;
begin
  i := ListBoxSizes.ItemIndex;
  if i>=0 then begin
    if x<=24 then
      FSizeInfos.Enabled[i] := not FSizeInfos.Enabled[i]
    else if x>ListBoxSizes.ClientWidth-24 then
      FSizeInfos.Delete(i);
  end;
end;

procedure TMainDialog.ListBoxSizesPaletteClick(Sender: TObject);
begin
  ComboBoxSize.Text := ListBoxSizesPalette.Items[ListBoxSizesPalette.ItemIndex];
end;

procedure TMainDialog.ListBoxSizesPaletteDblClick(Sender: TObject);
begin
  ButtonAddSizeClick(nil);
end;

procedure TMainDialog.ListBoxSizesPaletteDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  Canvas :TCanvas;
  tr, fr :TRect;
  Size :integer;
begin
  Size := FDefSizes[Index];
  Canvas := ListBoxSizesPalette.Canvas;

  // Background
  if odSelected in State then
    Canvas.Brush.Color := clHighlight
  else
    Canvas.Brush.Color := TSizeInfos.SIZENAMECOLORS[TSizeInfos.SizeToSizename(Size)];
  Canvas.FillRect(ARect);

  // Size
  tr := ARect;

  // Screen Marker
  if FScreenSize=Size then with ARect do begin
    fr := tr; fr.Inflate(-1, -2);
    Canvas.Pen.Color := TSizeInfos.SCREENSIZECOLOR;
    Canvas.Frame(fr);
  end;
  Canvas.Font.Size := 10;
  Canvas.Font.Style := [];
  Canvas.TextRect(tr, tr.Left, tr.Top, IntToStr(Size), TRDEFSIZE);

end;

procedure TMainDialog.ListBoxSizesPaletteMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  AHeight := ListBoxSizesPalette.ClientHeight div 4;
end;

procedure TMainDialog.ListBoxSizesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ListBoxSizes.ItemIndex=-1 then Exit;
  if Key = VK_DELETE then
    FSizeInfos.Delete(ListBoxSizes.ItemIndex)
  else if Key = VK_SPACE then
    FSizeInfos.Enabled[ListBoxSizes.ItemIndex] := not FSizeInfos.Enabled[ListBoxSizes.ItemIndex];
end;

procedure TMainDialog.ListBoxSizesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  Canvas :TCanvas;
  tr, fr :TRect;
  Size :integer;
begin

  // Layout:

  Size := FSizeInfos[Index].Size;
  Canvas := ListBoxSizes.Canvas;

  // Background
  Canvas.Brush.Color := IfThen(odSelected in State, clHighlight, TSizeInfos.SIZENAMECOLORS[TSizeInfos.SizeToSizename(Size)]);
  Canvas.FillRect(ARect);

  // CheckBox
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Left+2, ARect.Top+2, ifThen(FSizeInfos[Index].Enabled, 4, 3));

  // Size
  with ARect do tr := Rect(Left + 26, Top, Left+72, Bottom);

  // Screen Marker
  if FScreenSize=Size then begin
    fr := tr; fr.right += 4; fr.Inflate(-1, -2);
    Canvas.Pen.Color := TSizeInfos.SCREENSIZECOLOR;
    Canvas.Frame(fr);
  end;

  // Size
  Canvas.Font.Size := 10;
  Canvas.Font.Style := [fsbold];
  Canvas.TextRect(tr, tr.Left, tr.Top+2, IntToStr(Size), TRSIZE);

  // Size Icon
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Left+78, ARect.Top+2, integer(TSizeInfos.SizeToSizename(Size)));

  // SizeName
  with ARect do tr := Rect(Left + 106, Top, Right-26, Bottom);
  Canvas.Font.Style := [];
  Canvas.TextRect(tr, tr.Left, tr.Top+2, FSizeInfos[Index].Name, TRNAME);

  // Delete Symbol
  ImagesModule.ImageList24x24.Draw(Canvas, ARect.Right-24, ARect.Top+2, 5);

  // TopLine
  if Index>0 then begin
    Canvas.Pen.Color := clSilver;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Right, ARect.Top);
  end;
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
  LabelCurrentDir.Caption := Path;
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
begin
  if FIsDirty=AValue then Exit;
  FIsDirty:=AValue;
  ActionSave.ImageIndex := IfThen(Dirty, IMGIDX_DIRTY, IMGIDX_SAVE);
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
      RequiredSteps[2] := FSizeInfos.Required;
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
      Processor := TProcessor.Create;
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

        // Sizes
        Processor.Sizes := FSizeInfos.GetEnabledSizes;
        Processor.SizeNames := FSizeInfos.GetEnabledSizeNames;

        // Destination folder
        if (EditTargetFolder.Text='') then
          raise Exception.Create(SErrMissingDestinationFolder);

        // Quality
        if not TProcessor.TryStrToJPEGQuality(ComboBoxJPEGQuality.Text, IntValue) then
          raise Exception.Create(SErrInvalidJpgQuality);
        Processor.JPEGQuality := IntValue;
        Processor.PngCompression := ComboBoxPNGCompression.ItemIndex;
        Processor.Interpolation := TInterpolation(ComboBoxInterpolation.ItemIndex);

        // Sharpen
        if not TProcessor.TryStrToSharpen(ComboBoxSharpen.Text, IntValue) then
          raise Exception.Create(SErrInvalidSharpen);
        Processor.Sharpen := IntValue;

        // Rename
        if CheckBoxRenEnabled.Checked then begin
          if RadioButtonRenSimple.Checked then
            Processor.TargetFilename := RENSIMPLETEMPLATE
          else if RadioButtonRenAdvanced.Checked then
            Processor.TargetFilename := RENADVANCEDTEMPLATE
          else
            Processor.TargetFilename := EditRenTemplate.Text;
        end else
          Processor.TargetFilename := '';

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

        // stop, if SIZE placeholder is not contained either in
        // TargetFolder nor in TargetFilename
        TargetFolder := EditTargetFolder.Text;
        if (FSizeInfos.EnabledCount>1)
          and not templates.TEngine.ContainsOneOf(TargetFolder, ['SIZE', 'SIZENAME'], DOSDELIMITERS)
          and not (Processor.Rename and (templates.TEngine.ContainsOneOf(Processor.TargetFilename, ['SIZE', 'SIZENAME'], DOSDELIMITERS))) then
          raise Exception.Create(SErrEnterPlaceholder);

        // Hook the processor
        Processor.OnPrint := OnPrint;
        Processor.OnProgress := OnProgress;

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

