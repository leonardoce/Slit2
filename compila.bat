@echo off
echo Generating boostrap
echo -------------------
echo.
pushd bootstrap
fpc slit
popd

echo.
echo.
echo Recompiling slit
echo ----------------
echo.
if not exist bin mkdir bin
pushd bin
..\bootstrap\slit ..\slit.s
fpc slit.pas
.\slit ..\slit.s
fpc slit.pas
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
@echo on
