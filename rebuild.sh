#!/bin/sh
rm *.hi *.o
rm art/*
rm thumb/*
rm -rf state/Types.StereoidDb/
ghc -O2 Main.hs
runhaskell Conversion.hs
