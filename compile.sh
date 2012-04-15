#!/bin/bash
echo Generating boostrap
echo -------------------
echo
pushd bootstrap
fpc -gl slit
popd

echo
echo
echo Recompiling slit
echo ----------------
echo
if [ ! -e bin ]; then
  mkdir bin
fi
pushd bin
../bootstrap/slit ../slit.s
fpc -gl slit.pas
./slit ../slit.s
fpc -gl slit.pas
popd

echo
echo
echo Recompiling documentation
echo -------------------------
echo
pushd bin
cp ../book .
lout slit.s.lout -o slit.ps -r4
popd
