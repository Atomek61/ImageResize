@echo off
SET THEFILE=D:\Mf\DEV\Lazarus\ImageResize\bin\imageresize.exe
echo Linking %THEFILE%
D:\Programme\Lazarus\fpc\3.2.2\bin\x86_64-win64\ld.exe -b pei-x86-64  --gc-sections  -s --subsystem windows --entry=_WinMainCRTStartup    -o D:\Mf\DEV\Lazarus\ImageResize\bin\imageresize.exe D:\Mf\DEV\Lazarus\ImageResize\bin\link30344.res
if errorlevel 1 goto linkend
goto end
:asmend
echo An error occurred while assembling %THEFILE%
goto end
:linkend
echo An error occurred while linking %THEFILE%
:end
