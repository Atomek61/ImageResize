iscc ImageResize.iss
cd out
tar -acf InstallImageResize.zip InstallImageResize.exe
del InstallImageResize.exe
copy ..\..\bin\imgres.exe .
tar -acf imgres.zip imgres.exe
del imgres.exe
cd ..