unit huedlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  HColorPicker, mbColorPreview, mbColorConv, Logging;

type

  { TForm1 }

  TForm1 = class(TForm)
    HueColorPicker: THColorPicker;
    ColorPreviewSelected: TmbColorPreview;
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
//  Log('Color %8.8x', [HueColorPicker.Hue]);
  ColorPreviewSelected.Color := HSLToColor(HueColorPicker.Hue/360.0, 1.0, 0.5);
end;

end.

