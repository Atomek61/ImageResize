unit valueseditor;

{$mode delphi}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ValEdit, Dialogs, IniFiles, Generics.Collections, GetText,
  Graphics, FPImage, RegExpr, Grids, LCLType;

type

  { TCustomIniFileHelper }

  TCustomIniFileHelper = class helper for TCustomIniFile
    function StringRead(const Section, Key :string) :string;
    function ReadLang(const Section, Key, Lang :string) :string; overload;
    function ReadLang(const Section, Key, Default, Lang :string) :string; overload;
  end;

  TValuesEditor = class;

  { TEditor }

  TEditorClass = class of TEditor;

  TEditor = class
  private
    FId :string;
    FValuesEditor :TValuesEditor;
//    FItemProp :TItemProp;
  protected
    procedure Bind(ItemProp :TItemProp); virtual;
    property ValuesEditor :TValuesEditor read FValuesEditor;
//    property ItemProp :TItemProp read FItemProp;
  public
    Caption :string;
    Key :string;
    Text :string;
    Default :string;
    Hint :string;
    ReadOnly :boolean;
    constructor Create(const AId :string; AValuesEditor :TValuesEditor); virtual;
    class procedure Register(EditorClass :TEditorClass);
    procedure Load(Ini :TCustomIniFile; const Section :string); virtual;
    procedure ButtonClick; virtual;
    procedure DrawCell(const Value :string; Canvas :TCanvas; Rect :TRect; State :TGridDrawState); virtual;
    property Id :string read FId;
  end;

  { TStringEditor }

  TStringEditor = class(TEditor)
  protected
    procedure Bind(ItemProp :TItemProp); override;
  end;

  { TBooleanEditor }

  TBooleanEditor = class(TEditor)
  protected
    procedure Bind(ItemProp :TItemProp); override;
  end;

  { TWebColorEditor }

  TWebColorEditor = class(TEditor)
  protected
    procedure Bind(ItemProp :TItemProp); override;
    procedure ButtonClick; override;
    procedure DrawCell(const Value :string; Canvas :TCanvas; Rect :TRect; State :TGridDrawState); override;
  end;

  TEditorList = TObjectList<TEditor>;
  TEditorDictionary = TDictionary<string, TEditor>;

  { TValuesEditor }

  TValuesEditor = class
  private
    FList :TValueListEditor;
    FEditorList :TEditorList;
    FEditorDict :TEditorDictionary;
    FColorDialog :TColorDialog;
    FLanguage :string;
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
    procedure Load(Ini :TCustomIniFile; const Prefix :string);
    procedure Bind(ValueListEditor :TValueListEditor);
    property List :TValueListEditor read FList;
    property Editors :TEditorDictionary read FEditorDict;
  end;

  function ColorToWebColor(Value :TColor) :string;
  function TryWebColorToColor(const Value :string; out Color :TColor) :boolean;
  function WebColorToColor(const Value :string) :TColor;

resourcestring
  SErrSectionKeyNotFoundFmt = 'Key ''%s'' not found in section ''%s''.';
  SErrEditorClassNotFoundFmt = 'Editor class ''%s'' not registered.';
  SErrInvalidWebColorFmt = 'Invalid web color ''%s''.';
  SCptTrue = 'True';
  SCptFalse = 'False';

implementation

type
  TEditorClasses = TDictionary<string, TEditorClass>;

var
  EditorClasses :TEditorClasses;

function ColorToWebColor(Value :TColor) :string;
var
  Color :TFPColor;
  x :word;
begin
  Color := TColorToFPColor(ColorToRGB(Value));
  if Color.Alpha = High(word) then
    result := Format('#%2.2x%2.2x%2.2x', [byte(Color.Red), byte(Color.Green), byte(Color.Blue)])
  else
    result := Format('#%2.2x%2.2x%2.2x%2.2x', [byte(Color.Alpha), byte(Color.Red), byte(Color.Green), byte(Color.Blue)]);
end;

function TryWebColorToColor(const Value :string; out Color :TColor) :boolean;
var
  c :TFPColor;
  n :integer;

  function Hex1(i :integer) :word;
  begin
    result := ord(Value[i])-48;
    if result>9 then
      result := (ord(Value[i]) and not $20) - 55;
    result := result shl 8 + result;
  end;

  function Hex2(i :integer) :word;
  begin
    result := Hex1(i) shl 4 + Hex1(i+1);
  end;

  function IsHex :boolean;
  var
    i :integer;
    c :char;
  begin
    for i:=2 to n do begin
      c := Value[i];
      if not ((c>='0') and (c<='9') or (c>='A') and (c<='F') or (c>='a') and (c<='f')) then Exit(False);
    end;
    result := true;
  end;

begin
  n := Length(Value);
  result := (n>3) and (Value[1]='#') and IsHex;
  if result then begin
    case n of
    4:
      begin
        c.Alpha := $FFFF;
        c.red   := Hex1(2);
        c.green := Hex1(3);
        c.blue  := Hex1(4);
      end;
    5:
      begin
        c.Alpha := Hex1(2);
        c.red   := Hex1(3);
        c.green := Hex1(4);
        c.blue  := Hex1(5);
      end;
    7:
      begin
        c.Alpha := $FFFF;
        c.red   := Hex2(2);
        c.green := Hex2(4);
        c.blue  := Hex2(6);
      end;
    9:
      begin
        c.Alpha := Hex2(2);
        c.red   := Hex2(4);
        c.green := Hex2(6);
        c.blue  := Hex2(2);
      end;
    else
      Exit(False);
    end;
    Color := FPColorToTColor(c);
  end;
end;

function WebColorToColor(const Value: string): TColor;
begin
  if not TryWebColorToColor(Value, result) then
    raise Exception.CreateFmt(SErrInvalidWebColorFmt, [Value]);
end;

{ TEditor }

procedure TEditor.Bind(ItemProp: TItemProp);
begin
//  FItemProp := ItemProp;
end;

constructor TEditor.Create(const AId :string; AValuesEditor: TValuesEditor);
begin
  FId := AId;
  FValuesEditor := AValuesEditor;
end;

class procedure TEditor.Register(EditorClass: TEditorClass);
begin
  EditorClasses.Add(EditorClass.ClassName, EditorClass);
end;

procedure TEditor.Load(Ini: TCustomIniFile; const Section :string);
begin
  Caption := Ini.ReadLang(Section, 'Caption', FValuesEditor.FLanguage);
  Default := Ini.ReadLang(Section, 'Default', '', FValuesEditor.FLanguage);
  Text := Default;
end;

procedure TEditor.ButtonClick;
begin
//  FList.ColorDialog.Color :=
end;

procedure TEditor.DrawCell(const Value :string; Canvas: TCanvas; Rect: TRect; State: TGridDrawState);
begin

end;

{ TStringEditor }

procedure TStringEditor.Bind(ItemProp: TItemProp);
begin
  inherited;
  ItemProp.EditStyle := esSimple;
end;

{ TBooleanEditor }

procedure TBooleanEditor.Bind(ItemProp: TItemProp);
begin
  inherited;
  ItemProp.EditStyle := esPickList;
  ItemProp.PickList.Add(SCptFalse);
  ItemProp.PickList.Add(SCptTrue);
  ItemProp.ReadOnly := True;
end;

{ TWebColorEditor }

procedure TWebColorEditor.Bind(ItemProp: TItemProp);
begin
  ItemProp.EditStyle := esEllipsis;
end;

procedure TWebColorEditor.ButtonClick;
var
  ColorDialog :TColorDialog;
begin
  ColorDialog := ValuesEditor.ColorDialog;
  ColorDialog.Color := WebColorToColor(Text);
  if ColorDialog.Execute then begin
    Text := ColorToWebColor(ColorDialog.Color);
    ValuesEditor.FList.Values[Caption] := Text;
  end;
end;

procedure TWebColorEditor.DrawCell(const Value: string; Canvas: TCanvas;
  Rect: TRect; State: TGridDrawState);
var
  Color :TColor;
begin
  with Canvas do begin
    if TryWebColorToColor(Value, Color) then begin
      Brush.Color := Color;
      Rect.Left := Rect.Right - Rect.Height*3;
      inc(Rect.Top, 2); dec(Rect.Bottom, 2);
      FillRect(Rect);
    end;
  end;
//  inherited DrawCell(Value, Canvas, Rect, State);
end;

{ TCustomIniFileHelper }

function TCustomIniFileHelper.StringRead(const Section, Key: string): string;
begin
  result := ReadString(Section, Key, '<<>>');
  if result = '<<>>' then
    raise Exception.CreateFmt(SErrSectionKeyNotFoundFmt, [Key, Section]);
end;

function TCustomIniFileHelper.ReadLang(const Section, Key, Lang: string): string;
begin
  result := ReadString(Section, Key+'.'+Lang, '<<>>');
  if result = '<<>>' then
    result := StringRead(Section, Key);
end;

function TCustomIniFileHelper.ReadLang(const Section, Key, Default, Lang: string): string;
begin
  result := ReadString(Section, Key+'.'+Lang, '<<>>');
  if result = '<<>>' then begin
    result := ReadString(Section, Key, '<<>>');
    if result = '<<>>' then
      result := Default;
  end;
end;

{ TValuesEditor }

constructor TValuesEditor.Create;
var
  FallBackLang :string;
begin
  GetLanguageIDs(FLanguage, FallBackLang);
  FLanguage := Copy(FLanguage, 1, 2);
  FEditorList := TEditorList.Create;
  FEditorDict := TEditorDictionary.Create;
end;

destructor TValuesEditor.Destroy;
begin
  FEditorDict.Free;
  FEditorList.Free;
  FColorDialog.Free;
  inherited Destroy;
end;

procedure TValuesEditor.Bind(ValueListEditor: TValueListEditor);
var
  i :integer;
  ItemProp :TItemProp;
begin
  FList := ValueListEditor;
  FList.RowCount := FList.FixedRows;
  for i:=0 to FEditorList.Count-1 do with FEditorList[i] do begin
    FList.InsertRow(Caption, Text, true);
    ItemProp := FList.ItemProps[i];
    Bind(ItemProp);
  end;
  //FList.OnGetEditText := OnGetEditText;
  //FList.OnSetEditText := OnSetEditText;
  FList.OnButtonClick := OnButtonClick;
  FList.Invalidate;
  FList.OnDrawCell := OnDrawCell;
  FList.OnKeyDown := OnKeyDown;
end;

procedure TValuesEditor.Load(Ini: TCustomIniFile; const Prefix :string);
var
  Sections :TStringList;
  Section :string;
  Id :string;
  EditorClassName :string;
  Editor :TEditor;
  EditorClass :TEditorClass;
begin
  FEditorList.Clear;
  FEditorDict.Clear;
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for Section in Sections do begin
      if Section.StartsWith(Prefix+'.') then begin
        Id := Copy(Section, Length(Prefix)+2, Length(Section)-Length(Prefix)-1);
        EditorClassName := 'T'+Ini.StringRead(Section, 'Class')+'Editor';
        if not EditorClasses.TryGetValue(EditorClassName, EditorClass) then
          raise Exception.CreateFmt(SErrEditorClassNotFoundFmt, [EditorClassName]);
        Editor := EditorClass.Create(Id, self);
        try
          Editor.Load(Ini, Section);
          FEditorDict.Add(Id, Editor);
        except
          Editor.Free;
          raise;
        end;
        FEditorList.Add(Editor);
      end;
    end;
  finally
    Sections.Free;
  end;
end;

procedure TValuesEditor.OnGetEditText(Sender: TObject; ACol, ARow: Integer; var Value: string);
begin
  Value := FEditorList[ARow-1].Text;
end;

procedure TValuesEditor.OnSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  FEditorList[ARow-1].Text := Value;
end;

procedure TValuesEditor.OnButtonClick(Sender: TObject; ACol, ARow: Integer);
begin
  FEditorList[ARow-1].ButtonClick;
end;

procedure TValuesEditor.OnDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol=1) and (aRow>0) then
    FEditorList[ARow-1].DrawCell(FList.Cells[aCol, aRow], FList.Canvas, aRect, aState);
end;

procedure TValuesEditor.OnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (FList.Row>0) and (FList.Col=1) and (Key = VK_RETURN) and (ssCtrl in Shift) and (FList.ItemProps[FList.Row-1].EditStyle = esEllipsis) then begin
    FEditorList[FList.Row-1].ButtonClick;
  end;
end;

function TValuesEditor.ColorDialog: TColorDialog;
begin
  if not Assigned(FColorDialog) then
    FColorDialog := TColorDialog.Create(nil);
  result := FColorDialog;
end;

//procedure Test;
//var
//  c :TColor;
//  w :string;
//begin
//  c := clRed;
//  w := ColorToWebColor(c);
//  c := WebColorToColor(w);
//end;

initialization
begin
//  Test;
  EditorClasses := TEditorClasses.Create;
  TEditor.Register(TStringEditor);
  TEditor.Register(TBooleanEditor);
  TEditor.Register(TWebColorEditor);
end

finalization
begin
  EditorClasses.Free;
end

end.

