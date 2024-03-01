unit SettingsEditors;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLType, Generics.Collections, Grids, IniFiles, Graphics,
  Settings, ValEdit, Dialogs;

type
  TEditor = class;
  TEditorClass = class of TEditor;

  TEditorList = TObjectList<TEditor>;
  TEditorDictionary = TDictionary<string, TEditor>;

  { TSettingsEditor }

  // Binds a TValueListEditor to a list of TSetting and creates a TEditor to it.
  TSettingsEditor = class
  private
    FControl :TValueListEditor;
    FSettings :TSettings;
    FEditorList :TEditorList;
    FEditorDict :TEditorDictionary; // ? superfluous
    procedure OnButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure OnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind(ASettings :TSettings; AControl :TValueListEditor);
    procedure Flush;
    property Control :TValueListEditor read FControl;
    property Settings :TSettings read FSettings;
    property Editors :TEditorList read FEditorList;
  end;

  { TEditor }

  TEditor = class
  private
    FSettingsEditor :TSettingsEditor;
    FSetting :TSetting;
    FIndex :integer;
  protected
    procedure Bind(ItemProp :TItemProp); virtual;
    function GetCell: string;
    procedure SetCell(const Value: string);
    procedure DrawCell(Canvas :TCanvas; Rect :TRect; State :TGridDrawState); virtual;
    procedure ButtonClick; virtual;
    procedure ValueToCell; virtual;
    procedure CellToValue; virtual;
    property SettingsEditor :TSettingsEditor read FSettingsEditor;
    property Setting :TSetting read FSetting;
    property Index :integer read FIndex;
  public
    ReadOnly :boolean;
    constructor Create(ASetting :TSetting; ASettingsEditor :TSettingsEditor); virtual;
    class function ClassId :string;
    class procedure Register(const Classes :array of TEditorClass);
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    property Cell :string read GetCell write SetCell;
  end;

  { TStringEditor }

  TStringEditor = class(TEditor)
  end;

  { TInt32Editor }

  TInt32Editor = class(TStringEditor)
  public
  end;

  TUInt32Editor = class(TStringEditor)
  public
  end;


  { TPickEditor }

  TPickEditor = class(TEditor)
  protected
    procedure Bind(ItemProp :TItemProp); override;
  end;

  { TFilenameEditor }

  TFilenameEditor = class(TStringEditor)
  private
    FOpenDialog :TOpenDialog;
  protected
    procedure Bind(ItemProp :TItemProp); override;
    procedure ButtonClick; override;
  public
    destructor Destroy; override;
  end;

implementation

type
  TEditorClasses = TDictionary<string, TEditorClass>;

var
  EditorClasses :TEditorClasses;

{ TSettingsEditor }

constructor TSettingsEditor.Create;
begin
  FEditorList := TEditorList.Create;
  FEditorDict := TEditorDictionary.Create;
end;

destructor TSettingsEditor.Destroy;
begin
  FEditorList.Free;
  FEditorDict.Free;
  inherited Destroy;
end;

procedure TSettingsEditor.Bind(ASettings: TSettings; AControl: TValueListEditor);
var
  Setting :TSetting;
  EditorClassName :string;
  EditorClass :TEditorClass;
  i :integer;
begin
  FControl := AControl;
  FControl.OnButtonClick := OnButtonClick;
  FControl.OnDrawCell    := OnDrawCell;
  FControl.OnKeyDown     := OnKeyDown;
  FSettings := ASettings;
  FEditorList.Clear;
  FEditorDict.Clear;
  for i:=0 to Settings.Items.Count-1 do begin
    Setting := Settings.Items[i];
    EditorClassName := 'T'+Setting.PresentationHint+'Editor';
    if EditorClasses.TryGetValue(EditorClassName, EditorClass) then
      EditorClass.Create(Setting, self);
  end;
end;

procedure TSettingsEditor.Flush;
var
  Editor :TEditor;
begin
  for Editor in FEditorList do
    Editor.CellToValue;
end;

procedure TSettingsEditor.OnButtonClick(Sender: TObject; ACol, ARow: Integer);
var
  Index :integer;
begin
  Index := ARow-FControl.FixedRows;
  if (Index<0) or (Index>=FEditorList.Count) then Exit;
  FEditorList[Index].ButtonClick;
end;

procedure TSettingsEditor.OnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Index :integer;
begin
  if aCol<>0 then Exit;
  Index := ARow-FControl.FixedRows;
  if (Index<0) or (Index>=FEditorList.Count) then Exit;
  FEditorList[Index].DrawCell(FControl.Canvas, aRect, aState);
end;

procedure TSettingsEditor.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=VK_RETURN) and (Shift = [ssCtrl]) then
    OnButtonClick(Sender, FControl.Col, FControl.Row);
end;

{ TEditor }

constructor TEditor.Create(ASetting: TSetting; ASettingsEditor: TSettingsEditor);
begin
  FSettingsEditor := ASettingsEditor;
  FSetting := ASetting;
  FIndex := FSettingsEditor.FEditorList.Add(self);
  FSettingsEditor.FEditorDict.Add(FSetting.Key, self);
  FSettingsEditor.FControl.InsertRow(FSetting.Caption, '', true);
  Bind(FSettingsEditor.FControl.ItemProps[FIndex]);
  ValueToCell;
end;

class function TEditor.ClassId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-7);
end;

class procedure TEditor.Register(const Classes :array of TEditorClass);
var
  AClass :TEditorClass;
begin
  for AClass in Classes do
    EditorClasses.Add(AClass.ClassName, AClass);
end;

procedure TEditor.Load(Ini: TCustomIniFile; const Section: string);
begin

end;

procedure TEditor.ButtonClick;
begin

end;

procedure TEditor.ValueToCell;
begin
  Cell := FSetting.AsDisplay;
end;

procedure TEditor.CellToValue;
begin
  FSetting.AsDisplay := Cell;
end;

procedure TEditor.Bind(ItemProp :TItemProp);
begin
end;

function TEditor.GetCell: string;
begin
  result := FSettingsEditor.FControl.Cells[1, FIndex];
end;

procedure TEditor.SetCell(const Value: string);
begin
  if Value=GetCell then Exit;
  FSettingsEditor.FControl.Cells[1, FIndex] := Value;
end;

procedure TEditor.DrawCell(Canvas: TCanvas; Rect: TRect; State: TGridDrawState);
begin

end;

{ TPickEditor }

procedure TPickEditor.Bind(ItemProp: TItemProp);
var
  i :integer;
  PickSetting :TPickSetting;
begin
  inherited Bind(ItemProp);
  ItemProp.EditStyle := esPickList;
  PickSetting := Setting as TPickSetting;
  ItemProp.ReadOnly := PickSetting is TPicklistSetting;
  for i:=0 to High(PickSetting.Display) do
    ItemProp.PickList.Add(PickSetting.Display[i]);
end;

{ TFilenameEditor }

procedure TFilenameEditor.Bind(ItemProp: TItemProp);
begin
  inherited Bind(ItemProp);
  ItemProp.EditStyle := esEllipsis;
end;

procedure TFilenameEditor.ButtonClick;
begin
  inherited ButtonClick;
  if not Assigned(FOpenDialog) then
    FOpenDialog := TOpenDialog.Create(nil);
  FOpenDialog.Filename := Cell;
  if FOpenDialog.Execute then
    Cell := FOpenDialog.FileName;
end;

destructor TFilenameEditor.Destroy;
begin
  inherited Destroy;
  FOpenDialog.Free;
end;

{ TInt32Editor }

{ TStringEditor }

{ TInt32Editor }

initialization
begin
  EditorClasses := TEditorClasses.Create;
  TEditor.Register([TStringEditor, TInt32Editor, TUInt32Editor, TPickEditor, TFilenameEditor]);
end;

finalization
begin
  EditorClasses.Free;
end;

end.

