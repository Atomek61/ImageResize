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
  animator, logging, imagesmod, presentations,
  galleryprocessor, stringarrays, appsettings, 
presentationmanagerfrm, 
colorfrm, tagids, webutils,
settingseditor, webcolors, webcoloreditor, huedlg, settings2;

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

