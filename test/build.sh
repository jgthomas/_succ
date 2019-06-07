#!/bin/bash

main="LexerSpec.hs"
outfile="lexerTest"
include="../Lexer.hs ../Tokens.hs"

if [[ "$1" == "clean" ]]; then
        rm *.hi *.o &&
        find . -maxdepth 1 -type f ! -name "*.*" -delete
else
        ghc -dynamic -main-is "${main%.*}" --make $include "$main" -o "$outfile"
fi
