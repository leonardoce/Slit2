#!/bin/bash
if [ ! -e bin ]; then
  mkdir bin
fi
pushd bin
../../../bin/slit ../ped.s
cp ../book .
lout -r6 -o ped.ps ped.s.lout
popd
