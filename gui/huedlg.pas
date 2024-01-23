unit huedlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  BGRAGraphicControl, BCMaterialProgressBarMarquee, BCMaterialSpinEdit,
  BCMaterialFloatSpinEdit, HColorPicker;

type

  { TForm1 }

  TForm1 = class(TForm)
    HueColorPicker: THColorPicker;
    PanelSelectedColor: TPanel;
    procedure HueColorPickerChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.HueColorPickerChange(Sender: TObject);
begin
  PanelSelectedColor.Color := HueColorPicker.Value;
end;

end.

