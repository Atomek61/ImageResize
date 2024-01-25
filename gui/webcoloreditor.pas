unit webcoloreditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SettingsEditor, ValEdit, Graphics, Grids, Dialogs, WebUtils;

type

  { TWebColorEditor }

  TWebColorEditor = class(TInt32Editor)
  private
    FLock :boolean;
  protected
    procedure Bind(ItemProp :TItemProp); override;
    procedure DrawCell(Canvas :TCanvas; Rect :TRect; State :TGridDrawState); override;
    function GetPresentation: string; override;
    procedure SetPresentation(AValue: string); override;
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
    Brush.Color := StrToInt(Value);
    Rect.Left := Rect.Right - Rect.Height*2;
    dec(Rect.Right, 3);
    inc(Rect.Top, 2); dec(Rect.Bottom, 3);
    FillRect(Rect);
  end;
end;

function TWebColorEditor.GetPresentation: string;
begin
  result := ColorToHTMLColor(StrToInt(Value));
end;

procedure TWebColorEditor.SetPresentation(AValue: string);
var
  Color :TColor;
begin
  if not FLock and TryHTMLColorToColor(AValue, Color) then
    Value := IntToStr(Color);
end;

procedure TWebColorEditor.ButtonClick;
begin
  if not Assigned(ColorDialog) then
    ColorDialog := TColorDialog.Create(nil);
  ColorDialog.Color := StrToInt(Value);
  if ColorDialog.Execute then begin
    FLock := true;
    try
      Value := IntToStr(ColorDialog.Color);
      SetCell(GetPresentation);
    finally
      FLock := false;
    end;
  end;
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

