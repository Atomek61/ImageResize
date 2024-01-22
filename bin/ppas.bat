@echo off
SET THEFILE=D:\Mf\Lazarus\ImageResize\bin\imageresize.exe
echo Linking %THEFILE%
C:\lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o D:\Mf\Lazarus\ImageResize\bin\imageresize.exe D:\Mf\Lazarus\ImageResize\bin\link12072.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
