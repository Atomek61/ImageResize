@if [%1]==[] goto usage
..\bin\ImageResize.exe guitest%1.irs -A -X
@goto ok
:usage
@echo usage: guitest n
@echo   n is the test number from 1..5
:ok