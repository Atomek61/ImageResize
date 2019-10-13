#define PROGNAME "ImageResize"
#define PROGVER "2.4"

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
OutputDir=.
ArchitecturesAllowed=x64
ArchitecturesInstallIn64BitMode=x64
DisableWelcomePage=No
WizardImageFile=res\welcomelighthouse.bmp
OutputBaseFilename=Install{#PROGNAME}

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%n[name] is jpg and png image resampling tool.%nThere is a graphical user interface and a commandline interface available.

[Tasks]
Name: desktopicon; Description: "Create a desktop icon for the Windows application";
Name: htmlhelp; Description: "Install the HTML based helpfiles";
Name: installconsoleapp; Description: "Install the commandline tool imgres.exe";

[Run]
Filename: {app}\ImageResize.exe; Description: "Start {#PROGNAME} now"; Flags: postinstall; 

[Files]
Source: Files\ImageResize.exe; DestDir: "{app}"; 
Source: Files\imgres.exe; DestDir: "{win}"; Tasks: installconsoleapp; 
Source: Files\hlp\*.*; DestDir: "{app}\hlp"; Flags: recursesubdirs; Tasks: htmlhelp; 

[Icons]
Name: "{group}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}";
Name: "{commondesktop}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}"; Tasks: desktopicon; 
