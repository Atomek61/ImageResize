unit loggingrichmemo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, richmemo, graphics, logging, loggingtypes;

const
  RICHCOLORS :array[llHint..llCrash] of TColor = (clGreen, clWindowText, clOlive, clMaroon, clRed);

type

  { TRichMemoLogger }

  TRichMemoLogger = class(TLogger)
  private
    FRichMemo :TRichMemo;
  public
    constructor Create(RichMemo :TRichMemo);
    procedure Log(const Line :string; Level :TLogLevel = llInfo); override;
  end;

implementation

{ TRichMemoLogger }

constructor TRichMemoLogger.Create(RichMemo: TRichMemo);
begin
  inherited Create;
  FRichMemo := RichMemo;
end;

procedure TRichMemoLogger.Log(const Line: string; Level: TLogLevel);
begin
  FRichMemo.SetRangeColor(-1, -1, RICHCOLORS[Level]);
  FRichMemo.Lines.Add('Line: %s %d $%8.8x', [Line, integer(Level), longint(RICHCOLORS[Level])]);
end;

end.

