#!/bin/bash
ghc repa.hs -Odph -rtsopts -threaded -fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000 -fllvm -optlo-O3 -fforce-recomp && time ./repa +RTS -N > /dev/null && time ./repa > /dev/null
