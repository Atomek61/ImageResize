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
  Forms, lazcontrols, maindlg, settingsdlg, mrkeditdlg, presentationdlg, aboutdlg, imgres, updateutils,
  datetimeutils, threading.dispatcher, EXIFUtils, tags, settings,
  animator, logging, imagesmod, presentationprocessor,
  slideshow200presentationprocessor, galleryprocessor, stringarrays, appsettings, 
colorpresentationprocessorfrm, presentationprocessorfrm, 
colorfrm, imgtags, VarDisplay, tagids;

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

