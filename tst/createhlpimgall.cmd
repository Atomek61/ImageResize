FOR %%F IN (Stretch,Box,Linear,HalfCosine,Cosine,Bicubic,Mitchell,Spline,Lanczos2,Lanczos3,Lanczos4) DO (call createhlpimg testimage1.png 16 %%F)
..\bin\imgres tstHlp\tmp\*_16.png tstHlp\ 128 -f Stretch
FOR %%F IN (Stretch,Box,Linear,HalfCosine,Cosine,Bicubic,Mitchell,Spline,Lanczos2,Lanczos3,Lanczos4) DO (call createhlpimg testimage2.png 64 %%F)
..\bin\imgres tstHlp\tmp\*_64.png tstHlp\ 512 -f Stretch
FOR %%F IN (Stretch,Box,Linear,HalfCosine,Cosine,Bicubic,Mitchell,Spline,Lanczos2,Lanczos3,Lanczos4) DO (call createhlpimg testimage2.png 256 %%F)
..\bin\imgres tstHlp\tmp\*_256.png tstHlp\ 512 -f Stretch
del /Q ..\hlp\gui\img\img_*.png 
move tstHlp\img_*.png ..\hlp\gui\img