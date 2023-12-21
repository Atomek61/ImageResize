FOR %%F IN (Stretch,Box,Linear,HalfCosine,Cosine,Bicubic,Mitchell,Spline,Lanczos2,Lanczos3,Lanczos4) DO (call clitestfilter 64 %%F)
..\bin\imgres tstFilter\img_*_64.png ..\hlp\gui\img 512 -f Stretch
FOR %%F IN (Stretch,Box,Linear,HalfCosine,Cosine,Bicubic,Mitchell,Spline,Lanczos2,Lanczos3,Lanczos4) DO (call clitestfilter 256 %%F)
..\bin\imgres tstFilter\img_*_256.png ..\hlp\gui\img 512 -f Stretch
