unit settingseditor;

{$mode delphi}

interface

uses
  Classes, SysUtils, Generics.Collections, Dialogs, Grids, IniFiles, Graphics,
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
    property Control :TValueListEditor read FControl;
    property Settings :TSettings read FSettings;
  end;

  { TEditor }

  TEditor = class
  private
    FSettingsEditor :TSettingsEditor;
    FSetting :TSetting;
    procedure DrawCell(const Display :string; Canvas :TCanvas; Rect :TRect; State :TGridDrawState); virtual;
  protected
    procedure Bind(Index :integer); virtual;
    property SettingsEditor :TSettingsEditor read FSettingsEditor;
  public
    ReadOnly :boolean;
    constructor Create(ASetting :TSetting; ASettingsEditor :TSettingsEditor); virtual;
    class function ClassId :string;
    class procedure Register(const Classes :array of TEditorClass);
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    procedure ButtonClick; virtual;
  end;

  // For each TSetting derivate an equivalent exists

  { TStringEditor }

  TStringEditor = class(TEditor)
  protected
    procedure Bind(Index :integer); override;
  end;

  { TIntegerEditor }

  TIntegerEditor = class(TStringEditor)
  protected
    procedure Bind(Index :integer); override;
  end;

  { TWebColorEditor }

  TWebColorEditor = class(TIntegerEditor)
  protected
    procedure Bind(Index :integer); override;
  end;

  TPickEdit = class(TEditor)

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
  Editor :TEditor;
  i :integer;
begin
  FControl := AControl;
  FControl.OnGetEditText := OnGetCellDisplay;
  FControl.OnSetEditText := OnSetCellDisplay;
  FSettings := ASettings;
  FEditorList.Clear;
  FEditorDict.Clear;
  for i:=0 to Settings.Items.Count-1 do begin
    Setting := Settings.Items[i];
    EditorClassName := 'T'+Setting.Presentation+'Editor';
    if EditorClasses.TryGetValue(EditorClassName, EditorClass) then begin
//      FControl.RowCount := Settings.Count + FControl.FixedRows + 1;
      Editor := EditorClass.Create(Setting, self);
    end;
  end;
end;

procedure TSettingsEditor.OnGetCellDisplay(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  Value := FEditorList[ARow-FControl.FixedRows].FSetting.Display;
end;

procedure TSettingsEditor.OnSetCellDisplay(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FEditorList[ARow-FControl.FixedRows].FSetting.Display := Value;
end;

procedure TSettingsEditor.OnButtonClick(Sender: TObject; ACol, ARow: Integer);
begin

end;

procedure TSettingsEditor.OnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Index :integer;
begin
  if aCol<>1 then Exit;
  Index := ARow-FControl.FixedRows;
  if (Index<0) or (Index>=FEditorList.Count) then Exit;
    FEditorList[Index].DrawCell(FControl.Cells[aCol, aRow], FControl.Canvas, aRect, aState);
end;

procedure TSettingsEditor.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

{ TEditor }

constructor TEditor.Create(ASetting: TSetting; ASettingsEditor: TSettingsEditor);
var
  Index :integer;
begin
  FSettingsEditor := ASettingsEditor;
  FSetting := ASetting;
  Index := FSettingsEditor.FEditorList.Add(self);
  FSettingsEditor.FEditorDict.Add(ASetting.Key, self);
  FSettingsEditor.FControl.Strings.Add(FSetting.Caption+'='+FSetting.Display);
  Bind(Index);
end;

class function TEditor.ClassId: string;
begin
  result := Copy(ClassName, 2, Length(ClassName)-7);
end;

procedure TEditor.Bind(Index :integer);
begin
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

procedure TEditor.DrawCell(const Display :string; Canvas: TCanvas; Rect: TRect; State: TGridDrawState);
begin

end;

{ TStringEditor }

procedure TStringEditor.Bind(Index :integer);
begin
  inherited;
  with SettingsEditor.Control do begin
    ItemProps[Index].EditStyle := esSimple;
  end;
end;

{ TIntegerEditor }

procedure TIntegerEditor.Bind(Index: integer);
begin
  inherited;
  with SettingsEditor.Control do begin
    ItemProps[Index].EditStyle := esSimple;
  end;
end;

{ TWebColorEditor }

procedure TWebColorEditor.Bind(Index: integer);
begin
  inherited Bind(Index);
end;

initialization
begin
  EditorClasses := TEditorClasses.Create;
  TEditor.Register([TStringEditor, TIntegerEditor, TWebColorEditor]);
end;

finalization
begin
  EditorClasses.Free;
end;

end.

