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
  datetimeutils, threading.dispatcher, webcreatordlg, imggallery,
  EXIFUtils, tags, settingsdlg, settings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ImageResize';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainDialog, MainDialog);
  Application.CreateForm(TWebCreatorDialog, WebCreatorDialog);
  Application.CreateForm(TSettingsDialog, SettingsDialog);
  Application.Run;
end.

