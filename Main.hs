
module Main (main) where


import System.IO
import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
import Generator (generate)


main :: IO()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        print $ tokenize contents
        print $ parse $ tokenize contents
        writeFile "output.s" $ generate $ parse $ tokenize contents
        hClose handle
