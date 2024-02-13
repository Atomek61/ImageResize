unit controlshot;

{$mode ObjFPC}{$H+}

interface

uses
  LCLIntf, LCLType, Forms, Windows, Classes, SysUtils, Controls, Graphics, BGRABitmap, FPWritePNG;

var
  SnapshotFolder :string;

// Save a Snapshot of a part of a control. dx and dy may be negative and
// Height refers to the controls top-left corner
// Width is absolut, including the dx
// if width or height are 0, then they are extended to the controls size + 6
procedure Snapshot(const Filetitle :string; Control :TControl; dx, dy, Width, Height :integer);
procedure DelayForSnapshot(MilliSeconds :integer);

implementation

procedure Snapshot(const Filetitle :string; Control :TControl; dx, dy, Width, Height :integer);
var
  ScreenBmp :TBitmap;
  Bmp :TBitmap;
  SrcRect, DstRect :TRect;
  Form :TCustomForm;
  Bmpa :TBGRABitmap;
  PNGWriter :TFPWriterPNG;
  ScreenDC :HDC;

  function GetParentForm(Control :TControl) :TCustomForm;
  var
    Parent :TWinControl;
  begin
    Parent := Control.Parent;
    repeat
      if Parent is TCustomForm then
        Exit(Parent as TCustomForm);
      Parent := Parent.Parent;
    until not Assigned(Parent);
  end;

begin
  Application.ProcessMessages;
  if Width = 0 then Width := Control.Width - dx;
  if Height = 0 then Height := Control.Height;
  DstRect := Classes.Rect(0, 0, Width, Height-dy);
  SrcRect.TopLeft := Control.Parent.ClientToScreen(Control.BoundsRect.TopLeft);
  inc(SrcRect.Left, dx);
  inc(SrcRect.Top, dy);
  SrcRect.Right := SrcRect.Left + DstRect.Width;
  SrcRect.Bottom := SrcRect.Top + DstRect.Height;
  Bmpa := nil;
  PNGWriter := nil;
  ScreenBmp := TBitmap.Create;
  Bmp := TBitmap.Create;
  try
    ScreenDC := GetDC(0);
    ScreenBmp.LoadFromDevice(ScreenDC);
    ReleaseDC(0, ScreenDC);
    Bmp.SetSize(DstRect.Width, DstRect.Height);
    Bmp.Canvas.CopyRect(DstRect, ScreenBmp.Canvas, SrcRect);
    PNGWriter := TFPWriterPNG.Create;
    Bmpa := TBGRABitmap.Create(Bmp);
    Bmpa.SaveToFile(IncludeTrailingPathDelimiter(SnapshotFolder)+Filetitle+'.png', PngWriter);
  finally
    ScreenBmp.Free;
    Bmp.Free;
    Bmpa.Free;
    PNGWriter.Free;
  end;
end;

procedure DelayForSnapshot(MilliSeconds :integer);
var
  t0 :Int64;
begin
  t0 := GetTickCount64 + MilliSeconds;
  while GetTickCount64<t0 do begin
    Sleep(50);
    Application.ProcessMessages;
  end;
end;

end.

