
module Main (main) where


import System.IO
import System.Environment

import Lexer (tokenize)
import Parser (parse)


main :: IO()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        print $ tokenize contents
        print $ parse $ tokenize contents
        hClose handle
