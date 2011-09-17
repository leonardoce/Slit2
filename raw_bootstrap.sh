#!/usr/bin/bash
rm *.ppu
rm *.o

@echo off
../bootstrap/slit ../slit.s 
fpc -g -gl slit.pas 
./slit ../slit.s 
fpc slit.pas
