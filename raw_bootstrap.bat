@echo off
REM pensato per essere eseguito dalla bin
del *.ppu
del *.o
..\bootstrap\slit ..\slit.s & fpc -g -gl slit.pas & slit ..\slit.s & fpc slit.pas
