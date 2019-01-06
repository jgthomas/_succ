
module Main (main) where


import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)

import Lexer (tokenize)
import Parser (parse)
import Generator


main :: IO()
main = do
        args <- getArgs
        let infileName = head args
        let outfileName = dropExtension infileName
        print infileName
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle
        print $ tokenize contents
        print $ parse $ tokenize contents
        print $ generate $ parse $ tokenize contents
        let outfileText = progString $ generate $ parse $ tokenize contents
        print outfileText
        writeFile outfileName outfileText
        hClose handle
