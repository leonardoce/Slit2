@echo off
REM pensato per essere eseguito dalla bin
..\bootstrap\slit ..\slit.s & fpc -g -gl slit.pas & slit ..\slit.s & fpc slit.pas
