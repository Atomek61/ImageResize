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
  LCLIntf, Buttons, ImgList, LCLType;

const
  IMGRESGUIVER = '1.9';
  IMGRESGUICPR = 'ImageResize V'+IMGRESGUIVER+' © 2019 Jan Schirrmacher, www.atomek.de';

  INITYPE = 'IRS';
  INIVERSION = '100';

  LICENSE =
    'Image Resize Copyright (c) 2019 Jan Schirrmacher, www.atomek.de'#10#10+
    'Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify and merge copies of the Software, subject to the following conditions:'#10#10+
    'The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.'#10#10+
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';
  WEBURL = 'www.atomek.de/imageresize/index.html';

  REGKEY = '\Software\Atomek\ImageResize';

  LM_RUN = LM_USER + 1;

  DEFSIZES :array[0..10] of integer = (48, 120, 240, 360, 480, 640, 800, 960, 1200, 1920, 3840);


type

  { TMainDialog }

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TMainDialog = class(TForm)
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
    ButtonAbout: TBitBtn;
    ButtonClearSizes: TBitBtn;
    ButtonClearSizes1: TBitBtn;
    ButtonClearSizes2: TBitBtn;
    ButtonClearSrcFiles: TBitBtn;
    ButtonBrowseSrcFiles: TBitBtn;
    ButtonExecute: TBitBtn;
    BrowseDstFolder: TSelectDirectoryDialog;
    CheckBoxMrkEnabled: TCheckBox;
    ComboBoxBoost: TComboBox;
    ComboBoxMrkPosition: TComboBox;
    ComboBoxJpgQuality: TComboBox;
    ComboBoxPngCompression: TComboBox;
    ComboBoxSizes: TComboBox;
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
    Label18: TLabel;
    Label19: TLabel;
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
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionBrowseDstFolderExecute(Sender: TObject);
    procedure ActionBrowseFilenamesExecute(Sender: TObject);
    procedure ActionBrowseMrkFilenameExecute(Sender: TObject);
    procedure ActionClearFilenamesExecute(Sender: TObject);
    procedure ActionClearSizesExecute(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure ActionHelpExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ButtonExecuteClick(Sender: TObject);
    procedure ButtonMrkPos0Click(Sender: TObject);
    procedure ComboBoxMrkPositionChange(Sender: TObject);
    procedure ComboBoxMrkPositionDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure ComboBoxMrkPositionMeasureItem(Control: TWinControl;
      Index: Integer; var AHeight: Integer);
    procedure ComboBoxSizesChange(Sender: TObject);
    procedure EditDstFolderChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MemoSrcFilenamesChange(Sender: TObject);
    procedure PanelControlsClick(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
  private
    FAutoExit :boolean;
    FIsSave :boolean;
    FIniFilename :string;
    FImgRes :TImgRes;
    FCancelled :boolean;
    FExecuting :boolean;
    FProgress :single;
    FMrkPos :integer;
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnProgress(Sender :TObject; Progress :single);
    procedure SaveToRegistry;
    function LoadFromRegistry :boolean;
    procedure Save(const Filename :string);
    procedure Load(const Filename :string);
    procedure Log(const Msg :string);
    function MrkBorderToPos(const Value :TPos) :TPos;
    function MrkPosToBorder(const Value :TPos) :TPos;
    function BoostStrToThreadCount(const Value :string) :integer;
    function ThreadCountToBoostStr(ThreadCount :integer) :string;
    procedure UpdateControls;
    procedure LMRun(var Message: TLMessage); message LM_RUN;
    procedure SizeButtonClick(Sender :TObject);
    procedure SetMrkPos(Value :integer);
  public

  end;

var
  MainDialog: TMainDialog;

implementation

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

procedure TMainDialog.FormShow(Sender: TObject);
begin
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
  if Length(Trim(ComboBoxSizes.Text))=0 then
    ComboBoxSizes.Text := s
  else if TrySizesStrToSizes(ComboBoxSizes.Text + ', '+s, l) then
    ComboBoxSizes.Text := SizesToSizesStr(l);
end;

procedure TMainDialog.SetMrkPos(Value: integer);
begin
  FMrkPos := Value;
  ComboBoxMrkPosition.ItemIndex := Value;
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
    if Registry.OpenKey(REGKEY, true) then with Registry do begin
      WriteString('SrcFilenames', MemoSrcFilenames.Text);
      WriteString('DstFolder', EditDstFolder.Text);
      WriteString('Sizes', ComboBoxSizes.Text);
      WriteString('JpgOptions.Quality', ComboBoxJpgQuality.Text);
      WriteString('PngOptions.Compression', ComboBoxPngCompression.Text);
      WriteBool('MrkEnabled', CheckBoxMrkEnabled.Checked);
      WriteString('MrkFilename', EditMrkFilename.Text);
      WriteString('MrkSize', EditMrkSize.Text);
      WriteInteger('MrkPosition', FMrkPos);
      WriteString('MrkX', EditMrkX.Text);
      WriteString('MrkY', EditMrkY.Text);
      WriteString('MrkAlpha', EditMrkAlpha.Text);
      WriteString('ThreadCount', ComboBoxBoost.Text);
    end;
  finally
    Registry.CloseKey;
    Registry.Free;
  end;
end;

function TMainDialog.LoadFromRegistry :boolean;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_CURRENT_USER;
    // Navigate to proper "directory":
    with Registry do begin
      result := OpenKey(REGKEY, false);
      if result then begin
        MemoSrcFilenames.Text := ReadString('SrcFilenames');
        EditDstFolder.Text := ReadString('DstFolder');
        ComboBoxSizes.Text := ReadString('Sizes');
        ComboBoxJpgQuality.Text := ReadString('JpgOptions.Quality');
        ComboBoxPngCompression.Text := ReadString('PngOptions.Compression');
        CheckBoxMrkEnabled.Checked := ReadBool('MrkEnabled');
        EditMrkFilename.Text := ReadString('MrkFilename');
        EditMrkSize.Text := ReadString('MrkSize');
        SetMrkPos(ReadInteger('MrkPosition'));
        EditMrkX.Text := ReadString('MrkX');
        EditMrkY.Text := ReadString('MrkY');
        EditMrkAlpha.Text := ReadString('MrkAlpha');
        ComboBoxBoost.Text := ReadString('ThreadCount');
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
    ComboBoxSizes.Text := IntToStr(DEFAULTSIZE);
    ComboBoxJpgQuality.Text := Format('%.1d', [ImgResizer.JpgQuality]);
    ComboBoxPngCompression.Text := TImgRes.PngCompressionToStr(ImgResizer.PngCompression);
    CheckBoxMrkEnabled.Checked := false;
    EditMrkFilename.Text := '';
    SetMrkPos(2);
    EditMrkSize.Text := Format('%.1f', [ImgResizer.MrkSize]);
    with MrkPosToBorder(TPos.Create(ImgResizer.MrkX, ImgResizer.MrkY)) do begin
      EditMrkX.Text := Format('%.1f', [x]);
      EditMrkY.Text := Format('%.1f', [y]);
    end;
    EditMrkAlpha.Text := Format('%.1f', [ImgResizer.MrkAlpha]);
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
    Ini.WriteString('Settings', 'Sizes', ComboBoxSizes.Text);
    Ini.WriteString('Settings', 'JpgQuality', ComboBoxJpgQuality.Text);
    Ini.WriteString('Settings', 'PngCompression', ComboBoxPngCompression.Text);
    Ini.WriteInteger('Settings', 'MrkPosition', FMrkPos);
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
    ComboBoxSizes.Text := Ini.ReadString('Settings', 'Sizes', '');
    ComboBoxJpgQuality.Text := Ini.ReadString('Settings', 'JpgQuality', '');
    ComboBoxPngCompression.Text := Ini.ReadString('Settings', 'PngCompression', '');
    SetMrkPos(Ini.ReadInteger('Settings', 'MrkPosition', 0));
    CheckBoxMrkEnabled.Checked := Ini.ReadBool('Settings', 'MrkEnabled', false);
    EditMrkFilename.Text := Ini.ReadString('Settings', 'MrkFilename', '');
    EditMrkSize.Text := Ini.ReadString('Settings', 'MrkSize', '');
    EditMrkX.Text := Ini.ReadString('Settings', 'MrkX', '');
    EditMrkY.Text := Ini.ReadString('Settings', 'MrkY','');
    EditMrkAlpha.Text := Ini.ReadString('Settings', 'MrkAlpha', '');
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

function TMainDialog.MrkBorderToPos(const Value :TPos): TPos;
begin
  case FMrkPos of
  0: // TopLeft
    begin
      result := Value;
    end;
  1: // TopRight
    begin
      result.X := 100.0 - Value.X;
      result.Y := Value.Y;
    end;
  2: // BottomRight
    begin
      result.X := 100.0 - Value.X;
      result.Y := 100.0 - Value.Y;
    end;
  else // BottomLeft
    begin
      result.X := Value.X;
      result.Y := 100.0 - Value.Y;
    end;
  end;
end;

function TMainDialog.MrkPosToBorder(const Value :TPos): TPos;
begin
  result := MrkBorderToPos(Value);
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
  LabelSizesRequired.Enabled := Length(ComboBoxSizes.Text) = 0;
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

procedure TMainDialog.ButtonMrkPos0Click(Sender: TObject);
begin
  SetMrkPos((Sender as TToolButton).Tag);
end;

procedure TMainDialog.ComboBoxMrkPositionChange(Sender: TObject);
begin
  SetMrkPos(ComboBoxMrkPosition.ItemIndex);
end;

procedure TMainDialog.ComboBoxMrkPositionDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  DrawingStyle :TDrawingStyle;
  r :TRect;
  ts :TTextStyle;
begin
  with ComboBoxMrkPosition do begin
    if odSelected in State then
      DrawingStyle := dsSelected
    else
      DrawingStyle := dsNormal;
    Canvas.Brush.Color := clWindow;
    r := ARect; r.Right := r.Left + 24;
    Canvas.FillRect(r);
    if odSelected in State then
      Canvas.Brush.Color := clHighlight
    else
      Canvas.Brush.Color := clWindow;
    r.Left := r.Right;
    r.Right := ARect.Right;
    Canvas.FillRect(r);
    ImagelistMrkPositions.Draw(Canvas, ARect.Left, ARect.Top, Index, DrawingStyle, itImage, true);
    if odSelected in State then
      Canvas.Font.Color := clHighlightText
    else
      Canvas.Font.Color := clWindowText;
    with ts do begin
      Alignment := taLeftJustify;
      Layout := tlCenter;
      SingleLine := true;
      Clipping := false;
      ExpandTabs := false;
      ShowPrefix := false;
      Wordbreak := false;
      Opaque := false;
      SystemFont := false;
      RightToLeft := false;
      EndEllipsis := false;
    end;
    Canvas.TextRect(r, r.Left+4, 0, Items[Index], ts);
    if odFocused in State then
      Canvas.DrawFocusRect(ARect);
  end;
end;

procedure TMainDialog.ComboBoxMrkPositionMeasureItem(Control: TWinControl;
  Index: Integer; var AHeight: Integer);
begin
  AHeight := 24;
end;

procedure TMainDialog.MemoSrcFilenamesChange(Sender: TObject);
begin
  LabelSrcFilnamesRequired.Enabled := Length(MemoSrcFilenames.Text) = 0;
end;

procedure TMainDialog.PanelControlsClick(Sender: TObject);
begin

end;

procedure TMainDialog.EditDstFolderChange(Sender: TObject);
begin
 LabelDstFolderRequired.Enabled := Length(EditDstFolder.Text) = 0;
end;

procedure TMainDialog.ComboBoxSizesChange(Sender: TObject);
begin
  LabelSizesRequired.Enabled := Length(ComboBoxSizes.Text) = 0;
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
  ComboBoxSizes.Text := '';
  UpdateControls;
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
        if ComboBoxSizes.Text='default' then begin
          SetLength(Sizes, 1);
          Sizes[0] := 640;
        end else if not TrySizesStrToSizes(ComboBoxSizes.Text, Sizes) then
          raise Exception.Create('Invalid Sizes string.');
        FImgRes := TImgRes.Create;
        if ComboBoxJpgQuality.Text = 'default' then
          FImgRes.JpgQuality := 75
        else
          FImgRes.JpgQuality := StrToInt(ComboBoxJpgQuality.Text);
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

          with MrkBorderToPos(p) do begin
            FImgRes.MrkX := x;
            FImgRes.MrkY := y;
          end;

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

