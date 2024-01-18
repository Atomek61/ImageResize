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
    FColorDialog :TColorDialog;
    procedure OnGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
    procedure OnSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure OnButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure OnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure OnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    function ColorDialog :TColorDialog;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Bind(ASettings :TSettings; AControl :TValueListEditor);
    property Control :TValueListEditor read FControl;
    property Settings :TSettings read FSettings;
//    property Editors :TEditorDictionary read FEditorDict;
  end;

  { TEditor }

  TEditor = class
  private
    FSettingsEditor :TSettingsEditor;
    FSetting :TSetting;
  protected
    procedure Bind(ItemProp :TItemProp); virtual;
    property SettingsEditor :TSettingsEditor read FSettingsEditor;
//    property ItemProp :TItemProp read FItemProp;
  public
    Caption :string;
    Key :string;
    Text :string;
    Default :string;
    Hint :string;
    ReadOnly :boolean;
    constructor Create(ASetting :TSetting; ASettingsEditor :TSettingsEditor); virtual;
    class procedure Register(EditorClass :TEditorClass);
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    procedure ButtonClick; virtual;
    procedure DrawCell(const Value :string; Canvas :TCanvas; Rect :TRect; State :TGridDrawState); virtual;
  end;

  // For each TSetting derivate an equivalent exists
  TStringEdit = class(TEditor)

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

end;

destructor TSettingsEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TSettingsEditor.Bind(ASettings: TSettings; AControl: TValueListEditor);
var
  Setting :TSetting;
  EditorClassName :string;
  EditorClass :TEditorClass;
  Editor :TEditor;
begin
  FControl := AControl;
  FSettings := ASettings;
  FEditorList.Clear;
  FEditorDict.Clear;
  FControl.RowCount := Settings.Count;
  for Setting in Settings.Items do begin
    EditorClassName := 'T'+Copy(Setting.ClassName, 2, Length(Setting.ClassName)-9)+'Editor';
    if EditorClasses.TryGetValue(EditorClassName, EditorClass) then begin
      Editor := EditorClass.Create(Setting, self);
    end;
  end;
end;

procedure TSettingsEditor.OnGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin

end;

procedure TSettingsEditor.OnSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin

end;

procedure TSettingsEditor.OnButtonClick(Sender: TObject; ACol, ARow: Integer);
begin

end;

procedure TSettingsEditor.OnDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin

end;

procedure TSettingsEditor.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

function TSettingsEditor.ColorDialog: TColorDialog;
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
  Bind(FSettingsEditor.Control.ItemProps[Index]);
end;

procedure TEditor.Bind(ItemProp: TItemProp);
begin

end;

class procedure TEditor.Register(EditorClass: TEditorClass);
begin

end;

procedure TEditor.Load(Ini: TCustomIniFile; const Section: string);
begin

end;

procedure TEditor.ButtonClick;
begin

end;

procedure TEditor.DrawCell(const Value: string; Canvas: TCanvas; Rect: TRect;
  State: TGridDrawState);
begin

end;

initialization
begin
  EditorClasses := TEditorClasses.Create;
end;

finalization
begin
  EditorClasses.Free;
end;

end.

