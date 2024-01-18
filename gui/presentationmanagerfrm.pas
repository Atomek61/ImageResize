unit presentationmanagerfrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ValEdit;

type

  { TPresentationManagerFrame }

  TPresentationManagerFrame = class(TFrame)
    Label1: TLabel;
    ValueListEditor: TValueListEditor;
  private

  public

  end;

implementation

{$R *.lfm}

end.

