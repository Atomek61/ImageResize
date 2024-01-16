unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls,
  Grids, ExtCtrls, ValuesEditor, IniFiles, Types, LCLType;

type

  { TForm1 }

  TForm1 = class(TForm)
    ColorDialog1: TColorDialog;
    Edit1: TEdit;
    PanelEdit: TPanel;
    ve: TValueListEditor;
    procedure FormCreate(Sender: TObject);
    procedure veCheckboxToggled(Sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    procedure veKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure veSelectEditor(Sender: TObject; aCol, aRow: Integer;
      var Editor: TWinControl);
  private
    FValuesEditor :TValuesEditor;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  Ini :TCustomIniFile;
begin
  Ini := TIniFile.Create('D:\Mf\Dev\Lazarus\ImageResize\bin\presentations\pinboard100\pinboard100.prd');
  try
    FValuesEditor := TValuesEditor.Create;
    FValuesEditor.Load(Ini, 'DocValue');
    FValuesEditor.Bind(ve);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.veCheckboxToggled(Sender: TObject; aCol, aRow: Integer;
  aState: TCheckboxState);
begin
  Beep;
end;

procedure TForm1.veKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then
    Beep;
end;

procedure TForm1.veSelectEditor(Sender: TObject; aCol, aRow: Integer;
  var Editor: TWinControl);
begin
  if aRow=2 then begin
    Editor := Edit1;
    Editor.BoundsRect := ve.CellRect(aCol, aRow);
    Editor.Visible := true;
  end;
end;

end.

