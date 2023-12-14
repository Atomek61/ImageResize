unit webcreatordlg;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Types, imggallery;

type

  { TWebCreatorDialog }

  TWebCreatorDialog = class(TForm)
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    ListView1: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private

  public

  end;

var
  WebCreatorDialog: TWebCreatorDialog;

implementation

{$R *.lfm}

{ TWebCreatorDialog }

procedure TWebCreatorDialog.ListBox1DrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  r :TRect;
  CircleRect :TRect;
begin
  //r := ARect;
  //r.Right := r.Left + r.Height;
  //with ListBox1.Canvas do begin
  //  Pen.Color := clGray;
  //  Pen.Width := 2;
  //  Brush.Color := clRed;
  //  Ellipse(r);
  //end;
end;

procedure TWebCreatorDialog.FormCreate(Sender: TObject);
begin

end;

end.

