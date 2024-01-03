unit imagesmod;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

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

