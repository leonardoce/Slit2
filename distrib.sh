#!/bin/env bash

# This script will build all the file needed to distrubute
# Slit2 to the world!
sh compile.sh
pushd bin
ps2pdf slit.ps
zip slit-doc.zip slit.ps slit.pdf
zip slit-linux-x86.zip slit
popd

# Obviously, slit for win32 can't be compiled using this
# script...