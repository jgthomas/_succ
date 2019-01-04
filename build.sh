#!/bin/bash

main="Main.hs"
outfile="chask"

if [[ "$1" == "clean" ]]; then
        rm *.hi *.o &&
        find . -maxdepth 1 -type f ! -name "*.*" -delete
else
        ghc -dynamic --make "$main" -o "$outfile"
fi
