#!/bin/bash

for i in $(seq -f "%02g" 1 25)
do
  ghc jmt_haskell_$i.hs -O2
done

for i in $(seq -f "%02g" 1 25)
do
  echo "Day $i" 
  ./jmt_haskell_$i
  rm jmt_haskell_$i.hi
  rm jmt_haskell_$i.o
  rm jmt_haskell_$i
done
