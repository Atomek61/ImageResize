#define PROGNAME "ImageResize"
#define PROGVER "2.0"

#define PROGEXE "ImageResize.exe"

[Setup]
AppName={#PROGNAME}
AppVersion={#PROGVER}
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
Name: installconsoleapp; Description: "Install the commandline tool imres.exe";

[Run]
Filename: {app}\ImageResize.exe; Description: "Start {#PROGNAME} now"; Flags: postinstall; 

[Files]
Source: Files\ImageResize.exe; DestDir: "{app}"; 
Source: Files\imgres.exe; DestDir: "{win}"; Tasks: installconsoleapp; 

[Icons]
Name: "{group}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}";
Name: "{commondesktop}\{#PROGNAME}"; Filename: "{app}\{#PROGEXE}"; Tasks: desktopicon; 
