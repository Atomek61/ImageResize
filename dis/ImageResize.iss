#define PROGNAME "ImageResize"
#define PROGVER "3.4"

#define PROGEXE "ImageResize.exe"

[Setup]
AppName={#PROGNAME}
AppVersion={#PROGVER}
AppPublisher=Atomek - Jan Schirrmacher
AppPublisherURL=http://www.atomek.de
WizardStyle=modern
DefaultDirName={autopf}\{#PROGNAME}
DefaultGroupName=Atomek\{#PROGNAME}
UninstallDisplayIcon={app}\{#PROGEXE}
Compression=lzma2
SolidCompression=yes
SourceDir=..\
OutputDir=dis\out
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
DisableWelcomePage=No
WizardImageFile=dis\welcome.bmp
OutputBaseFilename=Install{#PROGNAME}

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%n[name] is a JPEG and PNG image resampling tool.%n- Available as GUI app or%n- as commandline interface

[Tasks]
Name: desktopicon; Description: "Create a desktop icon for the Windows application";
Name: localhelp; Description: "Install the HTML based helpfiles";
Name: installconsoleapp; Description: "Install the commandline tool imgres.exe";

[Run]
Filename: {app}\ImageResize.exe; Description: "Start {#PROGNAME} now"; Flags: postinstall; 

[Files]
Source: bin\ImageResize.exe; DestDir: "{app}"; 
Source: bin\locale\*.po; DestDir: "{app}\locale"; 
Source: bin\imgres.exe; DestDir: "{win}"; Tasks: installconsoleapp; 
Source: hlp\gui\*.*; DestDir: "{app}\hlp"; Flags: recursesubdirs; Tasks: localhelp; 

[Icons]
Name: "{group}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}";
Name: "{commondesktop}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}"; Tasks: desktopicon; 
