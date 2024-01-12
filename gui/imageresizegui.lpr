program imageresizegui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, maindlg, aboutdlg, mrkeditdlg, imgres, updateutils,
  datetimeutils, threading.dispatcher, EXIFUtils, tags, settingsdlg, settings,
  animator, logging, presentationdlg, imagesmod, presentationprocessor,
  slideshow200presentationprocessor, galleryprocessor, stringarrays,
colorpresentationprocessorfrm, presentationprocessorfrm, params, 
appsettings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ImageResize';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainDialog, MainDialog);
  Application.CreateForm(TSettingsDialog, SettingsDialog);
  Application.CreateForm(TPresentationDialog, PresentationDialog);
  Application.CreateForm(TImagesModule, ImagesModule);
  Application.Run;
  end.

