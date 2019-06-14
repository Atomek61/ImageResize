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
  Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ActnList, ExtCtrls, imgres, registry, aboutdlg, inifiles, strutils, LMessages,
  LCLIntf, Buttons, ImgList, LCLType, BGRABitmap, BGRABitmapTypes;

const
  IMGRESGUIVER = '2.0';
  IMGRESGUICPR = 'ImageResize V'+IMGRESGUIVER+' © 2019 Jan Schirrmacher, www.atomek.de';

  INITYPE = 'IRS';
  INIVERSION = '100';

  GUIREGKEY = REGKEY + '\Settings';

  MRKRECTRATIO = 3.0;

  LICENSE =
    'Image Resize Copyright (c) 2019 Jan Schirrmacher, www.atomek.de'#10#10+
    'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify and merge copies of the Software, subject to the following conditions:'#10#10+
    'The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.'#10#10+
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';
  WEBURL = 'www.atomek.de/imageresize/index.html';

  LM_RUN = LM_USER + 1;

  DEFSIZES :array[0..11] of integer = (48, 120, 240, 360, 480, 640, 800, 960, 1200, 1920, 2560, 3840);

type

  { TMainDialog }

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
    ButtonEditWatermark: TBitBtn;
    ButtonAbout: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonBrowseMrkFilename: TBitBtn;
    ButtonClearSizes2: TBitBtn;
    ButtonClearSrcFiles: TBitBtn;
    ButtonBrowseSrcFiles: TBitBtn;
    ButtonExecute: TBitBtn;
    BrowseDstFolder: TSelectDirectoryDialog;
    CheckBoxStopOnError: TCheckBox;
    CheckBoxMrkEnabled: TCheckBox;
    ComboBoxBoost: TComboBox;
    ComboBoxJpgQuality: TComboBox;
    ComboBoxPngCompression: TComboBox;
    EditSizes: TEdit;
    EditMrkX: TEdit;
    EditMrkFilename: TEdit;
    EditDstFolder: TEdit;
    EditMrkSize: TEdit;
    EditMrkY: TEdit;
    EditMrkAlpha: TEdit;
    FlowPanelSizeButtons: TFlowPanel;
    GroupBoxJpgOptions: TGroupBox;
    GroupBoxPngOptions: TGroupBox;
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
    LabelMrkSpace: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    LabelCores: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    LabelSizesRequired: TLabel;
    LabelSrcFilnamesRequired: TLabel;
    LabelDstFolderRequired: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    MemoMessages: TMemo;
    MemoSrcFilenames: TMemo;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PageControl: TPageControl;
    PaintBoxMrkPreview: TPaintBox;
    PanelPreview: TPanel;
    PanelMessages: TPanel;
    PanelControls: TPanel;
    ProgressBar: TProgressBar;
    SaveAsDialog: TSaveDialog;
    Splitter1: TSplitter;
    TabSheetRessources: TTabSheet;
    TabSheetSizes: TTabSheet;
    TabSheetQuality: TTabSheet;
    TabSheetWatermark: TTabSheet;
    TimerProgressBarOff: TTimer;
    ToolBar: TToolBar;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure MemoSrcFilenamesChange(Sender: TObject);
    procedure PaintBoxMrkPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMrkPreviewMouseLeave(Sender: TObject);
    procedure PaintBoxMrkPreviewMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxMrkPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMrkPreviewPaint(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
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
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnProgress(Sender :TObject; Progress :single);
    procedure SaveToRegistry;
    function LoadFromRegistry :boolean;
    procedure Save(const Filename :string);
    procedure Load(const Filename :string);
    procedure Log(const Msg :string);
    function BoostStrToThreadCount(const Value :string) :integer;
    function ThreadCountToBoostStr(ThreadCount :integer) :string;
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
  mrkeditdlg;

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
  Params :TStringArray;
  i :integer;
  Button :TButton;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  AllowDropFiles := true;
  if not LoadFromRegistry then
    ActionNew.Execute;
  UpdateControls;
  FAutoExit := Application.HasOption('X', 'AUTOEXIT');
  Params := Application.GetNonOptions('AX', ['AUTOSTART', 'AUTOEXIT']);
  if Length(Params)=1 then
    Load(Params[0]);
  if Application.HasOption('A', 'AUTOSTART') then
    PostMessage(Handle, LM_RUN, 0, 0);

  // Create Size Buttons
  for i:=0 to High(DEFSIZES) do begin
     Button := TButton.Create(self);
     Button.TabStop := false;
     Button.Caption := IntToStr(DEFSIZES[i]);
     Button.Parent := FlowPanelSizeButtons;
     Button.Tag := DEFSIZES[i];
     Button.OnClick := @SizeButtonClick;
     Button.Width := 58;
     Button.Height := 44;
     Button.Visible := true;
  end;

  PageControl.ActivePageIndex := 0;

  // Show number of cores
  LabelCores.Caption := Format('%d Cores', [TThread.ProcessorCount]);
end;

procedure TMainDialog.FormDropFiles(Sender: TObject; const FileNames: array of String);
begin
  MemoSrcFilenames.Lines.AddStrings(Filenames);
end;

procedure TMainDialog.LMRun(var Message: TLMessage);
begin
  ActionExecute.Execute;
  Application.ProcessMessages;
  if FAutoExit then begin
    Close;
  end;
end;

procedure TMainDialog.SizeButtonClick(Sender: TObject);
var
  s :string;
  l :TSizes;
begin
  s := IntToStr((Sender as TButton).Tag);
  if Length(Trim(EditSizes.Text))=0 then
    EditSizes.Text := s
  else if TrySizesStrToSizes(EditSizes.Text + ', '+s, l) then
    EditSizes.Text := SizesToSizesStr(l);
  UpdateControls;
end;

procedure TMainDialog.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveToRegistry;
end;

procedure TMainDialog.SaveToRegistry;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    // Navigate to proper "directory":
    if Registry.OpenKey(GUIREGKEY, true) then with Registry do begin
      WriteString('SrcFilenames', MemoSrcFilenames.Text);
      WriteString('DstFolder', EditDstFolder.Text);
      WriteString('Sizes', EditSizes.Text);
      WriteString('JpgOptions.Quality', ComboBoxJpgQuality.Text);
      WriteString('PngOptions.Compression', ComboBoxPngCompression.Text);
      WriteBool('MrkEnabled', CheckBoxMrkEnabled.Checked);
      WriteString('MrkFilename', EditMrkFilename.Text);
      WriteString('MrkSize', EditMrkSize.Text);
      WriteString('MrkX', EditMrkX.Text);
      WriteString('MrkY', EditMrkY.Text);
      WriteString('MrkAlpha', EditMrkAlpha.Text);
      WriteString('ThreadCount', ComboBoxBoost.Text);
      WriteBool('StopOnError', CheckBoxStopOnError.Checked);
    end;
  finally
    Registry.CloseKey;
    Registry.Free;
  end;
end;

function TMainDialog.LoadFromRegistry :boolean;
var
  Registry: TRegistry;

  procedure LoadUpDown(UpDown :TUpDown; const Name :string);
  var
    x :single;
  begin
    if TryStrToFloat(Registry.ReadString(Name), x) then
      UpDown.Position := round(x);
  end;

begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    // Navigate to proper "directory":
    with Registry do begin
      result := OpenKey(GUIREGKEY, false);
      if result then begin
        MemoSrcFilenames.Text := ReadString('SrcFilenames');
        EditDstFolder.Text := ReadString('DstFolder');
        EditSizes.Text := ReadString('Sizes');
        ComboBoxJpgQuality.Text := ReadString('JpgOptions.Quality');
        ComboBoxPngCompression.Text := ReadString('PngOptions.Compression');
        CheckBoxMrkEnabled.Checked := ReadBool('MrkEnabled');
        EditMrkFilename.Text := ReadString('MrkFilename');
        LoadUpDown(UpDownMrkSize, 'MrkSize');
        LoadUpDown(UpDownMrkX, 'MrkX');
        LoadUpDown(UpDownMrkY, 'MrkY');
        LoadUpDown(UpDownMrkAlpha, 'MrkAlpha');
        ComboBoxBoost.Text := ReadString('ThreadCount');
        CheckBoxStopOnError.Checked := ReadBool('StopOnError');

        SetTitle('last settings');
      end;
    end;
  finally
    Registry.Free;
  end;
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
    EditSizes.Text := IntToStr(DEFAULTSIZE);
    ComboBoxJpgQuality.Text := ImgResizer.JpgQualityToStr(ImgResizer.JpgQuality);
    ComboBoxPngCompression.Text := TImgRes.PngCompressionToStr(ImgResizer.PngCompression);
    CheckBoxMrkEnabled.Checked := false;
    EditMrkFilename.Text := '';
    UpDownMrkSize.Position := round(ImgResizer.MrkSize);
    UpDownMrkX.Position := round(ImgResizer.MrkX);
    UpDownMrkY.Position := round(ImgResizer.MrkY);
    UpDownMrkAlpha.Position := round(ImgResizer.MrkAlpha);
    FIsSave := false;
    FIniFilename := '';
    SetTitle('unnamed');
  finally
    ImgResizer.Free;
  end;
end;

procedure TMainDialog.ActionOpenExecute(Sender: TObject);
begin
  if OpenDialog.Execute then begin
    Load(OpenDialog.Filename);
  end;
end;

procedure TMainDialog.Save(const Filename: string);
var
  Ini :TIniFile;
begin
  Ini := TIniFile.Create(Filename);
  try
    Ini.WriteString('Common', 'Type', INITYPE);
    Ini.WriteString('Common', 'Version', INIVERSION);
    Ini.WriteString('Settings', 'SrcFilenames', ReplaceStr(MemoSrcFilenames.Text, #13#10, '|'));
    Ini.WriteString('Settings', 'DstFolder', EditDstFolder.Text);
    Ini.WriteString('Settings', 'Sizes', EditSizes.Text);
    Ini.WriteString('Settings', 'JpgQuality', ComboBoxJpgQuality.Text);
    Ini.WriteString('Settings', 'PngCompression', ComboBoxPngCompression.Text);
    Ini.WriteBool('Settings', 'MrkEnabled', CheckBoxMrkEnabled.Checked);
    Ini.WriteString('Settings', 'MrkFilename', EditMrkFilename.Text);
    Ini.WriteString('Settings', 'MrkSize', EditMrkSize.Text);
    Ini.WriteString('Settings', 'MrkX', EditMrkX.Text);
    Ini.WriteString('Settings', 'MrkY', EditMrkY.Text);
    Ini.WriteString('Settings', 'MrkAlpha', EditMrkAlpha.Text);
    FIsSave := true;
    FIniFilename := Filename;
    SetTitle(''''+FIniFilename+'''');
    Log(Format('Settings saved to ''%s''', [FIniFilename]));
  finally
     Ini.Free;
  end;
end;

procedure TMainDialog.Load(const Filename: string);
var
  Ini :TIniFile;
  IniVer :string;
  IniTyp :string;
begin
  if not FileExists(Filename) then
    raise Exception.CreateFmt('File ''%s'' not found.', [Filename]);
  Ini := TIniFile.Create(Filename);
  try
    IniTyp := Ini.ReadString('Common', 'Type', 'unknown');
    if IniTyp<>INITYPE then
      raise Exception.Create('Cant load Image Resize Settings File');
    IniVer := Ini.ReadString('Common', 'Version', '000');
    if IniVer<>INIVERSION then
      Log(Format('Warning: Unexpected ini file version %s, %s expected', [IniVer, INIVERSION]));
    MemoSrcFilenames.Text := ReplaceStr(Ini.ReadString('Settings', 'SrcFilenames', ''), '|', #13#10);
    EditDstFolder.Text := Ini.ReadString('Settings', 'DstFolder', '');
    EditSizes.Text := Ini.ReadString('Settings', 'Sizes', '');
    ComboBoxJpgQuality.Text := Ini.ReadString('Settings', 'JpgQuality', '');
    ComboBoxPngCompression.Text := Ini.ReadString('Settings', 'PngCompression', '');
    CheckBoxMrkEnabled.Checked := Ini.ReadBool('Settings', 'MrkEnabled', false);
    EditMrkFilename.Text := Ini.ReadString('Settings', 'MrkFilename', '');
    UpDownMrkSize.Position := Ini.ReadInteger('Settings', 'MrkSize', UpDownMrkSize.Position);
    UpDownMrkX.Position := Ini.ReadInteger('Settings', 'MrkX', UpDownMrkSize.Position);
    UpDownMrkY.Position := Ini.ReadInteger('Settings', 'MrkY', UpDownMrkY.Position);
    UpDownMrkAlpha.Position := Ini.ReadInteger('Settings', 'MrkAlpha', UpDownMrkAlpha.Position);
    FIsSave := true;
    FIniFilename := Filename;
    SetTitle(''''+Filename+'''');
  finally
     Ini.Free;
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
    Save(FIniFilename);
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
  sz, sx, sy, w, h :single;
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
    Save(SaveAsDialog.Filename);
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
begin
  if BrowseDstFolder.Execute then begin
    EditDstFolder.Text := BrowseDstFolder.FileName;
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
//  UpdateControls;
end;

procedure TMainDialog.ActionEditWatermarkExecute(Sender: TObject);
var
  MrkFilename :string;
begin
  if TMrkEditDialog.Execute(MrkFilename) then begin
    EditMrkFilename.Text := MrkFilename;
    EditMrkFilename.SelStart := Length(EditMrkFilename.Text);
    PageControl.ActivePage := TabSheetWatermark;
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
        if (MemoSrcFilenames.Text='') then
          raise Exception.Create('Missing source filenames.');
        if (EditDstFolder.Text='') then
          raise Exception.Create('Missing destination folder.');
        if EditSizes.Text='default' then begin
          SetLength(Sizes, 1);
          Sizes[0] := 640;
        end else if not TrySizesStrToSizes(EditSizes.Text, Sizes) then
          raise Exception.Create('Invalid Sizes string.');
        FImgRes := TImgRes.Create;
        if not TImgRes.TryStrToJpgQuality(ComboBoxJpgQuality.Text, IntValue) then
          raise Exception.Create('Invalid jpg quality.');
        FImgRes.JpgQuality := IntValue;
        FImgRes.PngCompression := ComboBoxPngCompression.ItemIndex;

        if CheckBoxMrkEnabled.Checked then begin

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
        end else
          FImgRes.MrkFilename := '';

        FImgRes.OnPrint := @OnPrint;
        FImgRes.OnProgress := @OnProgress;

        // If required, then append the %SIZE% placeholder
        DstFolder := EditDstFolder.Text;
        if (Length(Sizes)>1) and (Pos('%SIZE%', DstFolder)=0) then begin
          DstFolder := DstFolder + '%SIZE%';
          EditDstFolder.Text := DstFolder;
          Log('%SIZE% placeholder added to destination folder.');
        end;

        FImgRes.Sizes := SizesToSizesStr(Sizes);
        FImgRes.SrcFilenames := MemoSrcFilenames.Lines;
        FImgRes.DstFolder := DstFolder;
        FImgRes.ThreadCount := BoostStrToThreadCount(ComboBoxBoost.Text);
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
  OpenUrl('http://www.atomek.de/imageresize/index.html#gui');
end;

end.

