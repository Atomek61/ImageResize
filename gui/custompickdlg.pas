unit custompickdlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons;

type

  { TCustomPickDialog }

  TCustomPickDialog = class(TForm)
    BitBtn1: TBitBtn;
    PanelControls: TPanel;
  private

  public

  end;

var
  CustomPickDialog: TCustomPickDialog;

implementation

{$R *.lfm}

end.

