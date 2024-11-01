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
  Forms, FrameViewer09, lazcontrols, maindlg, settingsdlg, mrkeditdlg,
  presentationdlg, aboutdlg, imgres, updateutils, datetimeutils,
  threading.dispatcher, EXIFUtils, tags, settings, animator, logging, imagesmod,
  presentations, galleryprocessor, appsettings, presentationmanagerfrm,
  colorfrm, tagids, webutils, webcolors, webcoloreditor, settingseditors,
  language, inttypes, stringutils, imgutils, colorsetting, controlshot,
  templateengine, taggingdlg;

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
  Application.CreateForm(TTaggingDialog, TaggingDialog);
  Application.Run;
end.

