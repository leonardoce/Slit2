if not exist bin mkdir bin
pushd bin
..\..\..\bin\slit ..\ped.s
copy ..\book .
lout -r6 -o ped.ps ped.s.lout
popd
