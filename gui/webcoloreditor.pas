unit webcoloreditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SettingsEditors, ValEdit, Graphics, Grids, Dialogs, WebUtils;

type

  { TWebColorEditor }

  TWebColorEditor = class(TEditor)
  protected
    procedure Bind(ItemProp :TItemProp); override;
    procedure DrawCell(Canvas :TCanvas; Rect :TRect; State :TGridDrawState); override;
    procedure ButtonClick; override;
  end;

implementation

var
  ColorDialog :TColorDialog = nil;

{ TWebColorEditor }

procedure TWebColorEditor.Bind(ItemProp: TItemProp);
begin
  inherited;
  ItemProp.EditStyle := esEllipsis;
end;

procedure TWebColorEditor.DrawCell(Canvas: TCanvas; Rect: TRect; State: TGridDrawState);
begin
  with Canvas do begin
    Brush.Color := HTMLColorToColor(Cell);
    Rect.Left := Rect.Right - Rect.Height*2;
    dec(Rect.Right, 3);
    inc(Rect.Top, 2); dec(Rect.Bottom, 3);
    FillRect(Rect);
  end;
end;

procedure TWebColorEditor.ButtonClick;
begin
  if not Assigned(ColorDialog) then
    ColorDialog := TColorDialog.Create(nil);
  ColorDialog.Color := HTMLColorToColor(Cell);
  if ColorDialog.Execute then
    Cell := ColorToHTMLColor(ColorDialog.Color);
end;

initialization
begin
  TEditor.Register([TWebColorEditor]);
end;

finalization
begin
  ColorDialog.Free;
end;

end.

