unit settingseditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, LCLType, Generics.Collections, Grids, IniFiles, Graphics,
  Settings, ValEdit;

type
  TEditor = class;
  TEditorClass = class of TEditor;

  TEditorList = TObjectList<TEditor>;
  TEditorDictionary = TDictionary<string, TEditor>;

  { TSettingsEditor }

  TSettingsEditor = class
  private
    FControl :TValueListEditor;
    FSettings :TSettings;
    FEditorList :TEditorList;
    FEditorDict :TEditorDictionary;
    procedure OnGetCellDisplay(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure OnSetCellDisplay(Sender: TObject; ACol, ARow: Integer; const Value: string);
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
    FValue :string;
  protected
    procedure Bind(ItemProp :TItemProp); virtual;
    function GetPresentation: string; virtual;
    procedure SetPresentation(AValue: string); virtual;
    procedure DrawCell(Canvas :TCanvas; Rect :TRect; State :TGridDrawState); virtual;
    procedure ButtonClick; virtual;
    procedure SetCell(const AValue :string);
    procedure Flush; virtual;
    property SettingsEditor :TSettingsEditor read FSettingsEditor;
    property Setting :TSetting read FSetting;
    property Value :string read FValue write FValue;
    property Index :integer read FIndex;
  public
    ReadOnly :boolean;
    constructor Create(ASetting :TSetting; ASettingsEditor :TSettingsEditor); virtual;
    class function ClassId :string;
    class procedure Register(const Classes :array of TEditorClass);
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
  end;

  // For each TSetting derivate an equivalent exists

  { TStringEditor }

  TStringEditor = class(TEditor)
  end;

  { TInt32Editor }

  TInt32Editor = class(TStringEditor)
  end;

  TUInt32Editor = class(TStringEditor)
  end;


  { TPickEditor }

  TPickEditor = class(TEditor)
  public
    procedure Bind(ItemProp :TItemProp); override;
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
  FControl.OnGetEditText := OnGetCellDisplay;
  FControl.OnSetEditText := OnSetCellDisplay;
  FControl.OnButtonClick := OnButtonClick;
  FControl.OnDrawCell    := OnDrawCell;
  FControl.OnKeyDown     := OnKeyDown;
  FSettings := ASettings;
  FEditorList.Clear;
  FEditorDict.Clear;
  for i:=0 to Settings.Items.Count-1 do begin
    Setting := Settings.Items[i];
    EditorClassName := 'T'+Setting.Presentation+'Editor';
    if EditorClasses.TryGetValue(EditorClassName, EditorClass) then
      EditorClass.Create(Setting, self);
  end;
end;

procedure TSettingsEditor.Flush;
var
  Editor :TEditor;
begin
  for Editor in FEditorList do
    Editor.Flush;
end;

procedure TSettingsEditor.OnGetCellDisplay(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  Value := FEditorList[ARow-FControl.FixedRows].GetPresentation;
end;

procedure TSettingsEditor.OnSetCellDisplay(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FEditorList[ARow-FControl.FixedRows].SetPresentation(Value);
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
  FSettingsEditor.FEditorDict.Add(ASetting.Key, self);
  FValue := FSetting.AsDisplay;
  FSettingsEditor.FControl.Strings.Add(FSetting.Caption+'='+GetPresentation);
  Bind(FSettingsEditor.FControl.ItemProps[FIndex]);
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

procedure TEditor.SetCell(const AValue: string);
begin
  SettingsEditor.Control.Values[Setting.Caption] := AValue;
end;

procedure TEditor.Flush;
begin
  FSetting.AsDisplay := Value;
end;

procedure TEditor.Bind(ItemProp :TItemProp);
begin
end;

function TEditor.GetPresentation: string;
begin
  result := FValue;
end;

procedure TEditor.SetPresentation(AValue: string);
begin
  FValue := AValue;
end;

procedure TEditor.DrawCell(Canvas: TCanvas; Rect: TRect; State: TGridDrawState);
begin

end;

{ TPickEditor }

procedure TPickEditor.Bind(ItemProp: TItemProp);
var
  i :integer;
  Pick :TPickSetting;
begin
  inherited Bind(ItemProp);
  ItemProp.EditStyle := esPickList;
  Pick := Setting as TPickSetting;
  ItemProp.ReadOnly := Pick is TPicklistSetting;
  for i:=0 to High(Pick.Display) do
    ItemProp.PickList.Add(Pick.Display[i]);
end;

{ TInt32Editor }

{ TStringEditor }

{ TInt32Editor }

initialization
begin
  EditorClasses := TEditorClasses.Create;
  TEditor.Register([TStringEditor, TInt32Editor, TUInt32Editor, TPickEditor]);
end;

finalization
begin
  EditorClasses.Free;
end;

end.

