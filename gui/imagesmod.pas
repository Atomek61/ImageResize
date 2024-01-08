unit imagesmod;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics;

const
  PANELLIGHTCOLOR = $00F1DDC9;
  PANELDARKCOLOR  = $00E9C9A9;

  PANELCOLORS :array[boolean] of TColor = (PANELLIGHTCOLOR, PANELDARKCOLOR);

type

  { TImagesModule }

  TImagesModule = class(TDataModule)
    ImageList20x20: TImageList;
  private

  public

  end;

var
  ImagesModule: TImagesModule;

implementation

{$R *.lfm}

end.

