unit maindlg;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, Types, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ActnList, ExtCtrls, imgres, registry, aboutdlg, inifiles, strutils;

const
  IMGRESGUIVER = '1.2';
  IMGRESGUICPR = 'ImageResize V'+IMGRESGUIVER+' © 2019 Jan Schirrmacher, www.atomek.de';

  INITYPE = 'IRS';
  INIVERSION = '100';

  LICENSE =
    'imgres.exe and ImageResize.exe (c) 2019 by Jan Schirrmacher, www.atomek.de'#10+
    'imgres.exe and ImageResize.exe are free software. You can use it for'#10+
    'any purpose private or commercial except selling it or disclaiming it'#10+
    'as your work.'#10#10+
    'There is no warranty for the results of this work.';

  WEBURL = 'www.atomek.de/imageresize/index.html';

  REGKEY = '\Software\Atomek\ImageResize';

type

  { TMainDialog }

  { TPos }

  TPos = record
    X, Y :single;
    constructor Create(ax, ay :single);
  end;

  TMainDialog = class(TForm)
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
    BrowseDstFolder: TSelectDirectoryDialog;
    Button1: TButton;
    ButtonAbout: TButton;
    ButtonClearSizes: TButton;
    ButtonBrowseWatermarkFilename: TButton;
    ButtonBrowseFilenames: TButton;
    ButtonBrowseFolder: TButton;
    ButtonClearFilenames: TButton;
    CheckBoxMrkEnabled: TCheckBox;
    ComboBoxMrkPos: TComboBox;
    ComboBoxJpgQuality: TComboBox;
    ComboBoxPngCompression: TComboBox;
    ComboBoxSizes: TComboBox;
    EditMrkX: TEdit;
    EditMrkFilename: TEdit;
    EditDstFolder: TEdit;
    EditMrkSize: TEdit;
    EditMrkY: TEdit;
    EditMrkAlpha: TEdit;
    GroupBox1: TGroupBox;
    GroupBoxJpgOptions: TGroupBox;
    GroupBoxPngOptions: TGroupBox;
    ImageList: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    LabelSrcFilnamesRequired: TLabel;
    LabelDstFolderRequired: TLabel;
    Label2: TLabel;
    LabelSizesRequired: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MemoMessages: TMemo;
    MemoSrcFilenames: TMemo;
    OpenDialog: TOpenDialog;
    OpenDialogSrcFilenames: TOpenDialog;
    OpenDialogMrkFilename: TOpenDialog;
    PanelMessages: TPanel;
    PanelControls: TPanel;
    ProgressBar: TProgressBar;
    SaveAsDialog: TSaveDialog;
    Splitter1: TSplitter;
    TimerProgressBarOff: TTimer;
    ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionBrowseDstFolderExecute(Sender: TObject);
    procedure ActionBrowseFilenamesExecute(Sender: TObject);
    procedure ActionBrowseMrkFilenameExecute(Sender: TObject);
    procedure ActionClearFilenamesExecute(Sender: TObject);
    procedure ActionClearSizesExecute(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure ActionNewExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionSaveAsExecute(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ComboBoxSizesChange(Sender: TObject);
    procedure EditDstFolderChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MemoSrcFilenamesChange(Sender: TObject);
    procedure PanelControlsClick(Sender: TObject);
    procedure TimerProgressBarOffTimer(Sender: TObject);
  private
    FIsSave :boolean;
    FIniFilename :string;
    FImgRes :TImgRes;
    FCancelled :boolean;
    FExecuting :boolean;
    FProgress :single;
    procedure SetTitle(const Str :string);
    procedure OnPrint(Sender :TObject; const Line :string);
    procedure OnCanProgress(Sender :TObject; Progress :single; var Cancel :boolean);
    procedure SaveToRegistry;
    function LoadFromRegistry :boolean;
    procedure Save(const Filename :string);
    procedure Log(const Msg :string);
    function MrkBorderToPos(const Value :TPos) :TPos;
    function MrkPosToBorder(const Value :TPos) :TPos;
    procedure UpdateControls;
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
begin
  DefaultFormatSettings.DecimalSeparator := '.';
  if not LoadFromRegistry then
    ActionNew.Execute;
  UpdateControls;
end;

procedure TMainDialog.PanelControlsClick(Sender: TObject);
begin

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
      WriteString('MrkPosition', ComboBoxMrkPos.Text);
      WriteString('MrkX', EditMrkX.Text);
      WriteString('MrkY', EditMrkY.Text);
      WriteString('MrkAlpha', EditMrkAlpha.Text);
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
        ComboBoxMrkPos.Text := ReadString('MrkPosition');
        EditMrkX.Text := ReadString('MrkX');
        EditMrkY.Text := ReadString('MrkY');
        EditMrkAlpha.Text := ReadString('MrkAlpha');
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
    ComboBoxMrkPos.ItemIndex := 3;
    EditMrkSize.Text := Format('%.1f', [ImgResizer.WatermarkSize]);
    with MrkPosToBorder(TPos.Create(ImgResizer.WatermarkX, ImgResizer.WatermarkY)) do begin
      EditMrkX.Text := Format('%.1f', [x]);
      EditMrkY.Text := Format('%.1f', [y]);
    end;
    EditMrkAlpha.Text := Format('%.1f', [ImgResizer.WatermarkAlpha]);
    FIsSave := false;
    FIniFilename := '';
    SetTitle('unnamed');
  finally
    ImgResizer.Free;
  end;
end;

procedure TMainDialog.ActionOpenExecute(Sender: TObject);
var
  Ini :TIniFile;
  IniVer :string;
  IniTyp :string;
begin
  if OpenDialog.Execute then begin
    Ini := TIniFile.Create(OpenDialog.Filename);
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
      ComboBoxMrkPos.ItemIndex := Ini.ReadInteger('Settings', 'MrkPosition', 0);
      CheckBoxMrkEnabled.Checked := Ini.ReadBool('Settings', 'MrkEnabled', false);
      EditMrkFilename.Text := Ini.ReadString('Settings', 'MrkFilename', '');
      EditMrkSize.Text := Ini.ReadString('Settings', 'MrkSize', '');
      EditMrkX.Text := Ini.ReadString('Settings', 'MrkX', '');
      EditMrkY.Text := Ini.ReadString('Settings', 'MrkY','');
      EditMrkAlpha.Text := Ini.ReadString('Settings', 'MrkAlpha', '');
      FIsSave := true;
      FIniFilename := OpenDialog.Filename;
      SetTitle(''''+FIniFilename+'''');
    finally
       Ini.Free;
    end;
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
    Ini.WriteInteger('Settings', 'MrkPosition', ComboBoxMrkPos.ItemIndex);
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

procedure TMainDialog.Log(const Msg: string);
begin
  MemoMessages.Lines.Add(Msg);
end;

function TMainDialog.MrkBorderToPos(const Value :TPos): TPos;
begin
  case ComboBoxMrkPos.ItemIndex of
  1: // TopRight
    begin
      result.X := 100.0 - Value.X;
      result.Y := Value.Y;
    end;
  2: // BottomRight
    begin
      result.X := Value.X;
      result.Y := 100.0 - Value.Y;
    end;
  3:
    begin
      result.X := 100.0 - Value.X;
      result.Y := 100.0 - Value.Y;
    end;
  else // TopLeft
    begin
      result := Value;
    end;
  end;
end;

function TMainDialog.MrkPosToBorder(const Value :TPos): TPos;
begin
  result := MrkBorderToPos(Value);
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

procedure TMainDialog.MemoSrcFilenamesChange(Sender: TObject);
begin
  LabelSrcFilnamesRequired.Enabled := Length(MemoSrcFilenames.Text) = 0;
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
  MemoSrcFilenames.Clear;
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
end;

procedure TMainDialog.OnCanProgress(Sender: TObject; Progress: single; var Cancel: boolean);
begin
  FProgress := Progress;
  ProgressBar.Position := round(Progress*100.0);
  Application.ProcessMessages;
  Cancel := FCancelled;
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
    ActionExecute.ImageIndex := 5;
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
        end else if not SizesStrToSizes(ComboBoxSizes.Text, Sizes) then
          raise Exception.Create('Invalid Sizes string.');
        FImgRes := TImgRes.Create;
        if ComboBoxJpgQuality.Text = 'default' then
          FImgRes.JpgQuality := 75
        else
          FImgRes.JpgQuality := StrToInt(ComboBoxJpgQuality.Text);
        FImgRes.PngCompression := ComboBoxPngCompression.ItemIndex;

        if CheckBoxMrkEnabled.Checked then begin

          FImgRes.WatermarkFilename := EditMrkFilename.Text;

          if not TryStrToFloat(EditMrkSize.Text, x) or (x<0.0) or (x>100.0) then
            raise Exception.Create('Invalid watermark size.');
          FImgRes.WatermarkSize := x;

          if not TryStrToFloat(EditMrkX.Text, p.x) or (p.x<0.0) or (p.x>100.0) then
            raise Exception.Create('Invalid watermark x border.');
          if not TryStrToFloat(EditMrkY.Text, p.y) or (p.y<0.0) or (p.y>100.0) then
            raise Exception.Create('Invalid watermark y border.');

          with MrkBorderToPos(p) do begin
            FImgRes.WatermarkX := x;
            FImgRes.WatermarkY := y;
          end;

          if not TryStrToFloat(EditMrkAlpha.Text, x) or (x<0.0) or (x>100.0) then
            raise Exception.Create('Invalid watermark opacity.');
          FImgRes.WatermarkAlpha := x;
        end else
          FImgRes.WatermarkFilename := '';

        FImgRes.OnPrint := @OnPrint;
        FImgRes.OnCanProgress := @OnCanProgress;

        // If required, then append the %SIZE% placeholder
        DstFolder := EditDstFolder.Text;
        if (Length(Sizes)>1) and (Pos('%SIZE%', DstFolder)=0) then begin
          DstFolder := DstFolder + '%SIZE%';
          EditDstFolder.Text := DstFolder;
          Log('%SIZE% placeholder added to destination folder.');
        end;

        FImgRes.Execute(MemoSrcFilenames.Lines, DstFolder, Sizes);

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
      ActionExecute.ImageIndex := 4;
      TimerProgressBarOff.Enabled := true;
    end;
  end;

end;

end.

