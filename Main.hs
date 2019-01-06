
module Main (main) where


import System.IO
import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
import Generator


main :: IO()
main = do
        args <- getArgs
        handle <- openFile (head args) ReadMode
        contents <- hGetContents handle
        print $ tokenize contents
        print $ parse $ tokenize contents
        print $ generate $ parse $ tokenize contents
        let outfileText = progString $ generate $ parse $ tokenize contents
        print outfileText
        writeFile "output.s" outfileText
        hClose handle
