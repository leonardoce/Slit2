@echo off
echo Generating boostrap
echo -------------------
echo.
pushd bootstrap
fpc -gl slit
popd

echo.
echo.
echo Recompiling slit
echo ----------------
echo.
if not exist bin mkdir bin
pushd bin
..\bootstrap\slit ..\slit.s
fpc -gl slit.pas
if %ERRORLEVEL%==1 popd & goto end
.\slit ..\slit.s
fpc -gl slit.pas
popd

echo.
echo.
echo Recompiling documentation
echo -------------------------
echo.
pushd bin
copy ..\book .
lout slit.s.lout -o slit.ps -r4
popd

:end
@echo on
