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
popd

@echo on
