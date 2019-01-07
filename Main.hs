
module Main (main) where


import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)

import Lexer (tokenize)
import Parser (parse)
import Generator (genASM)


main :: IO()
main = do
        args <- getArgs
        let infileName = head args
        let outfileName = (dropExtension infileName) ++ ".s"
        print infileName
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle

        let assembly = genASM $ parse $ tokenize contents

        if (length assembly > 0)
           then writeFile outfileName assembly
           else error "error"

        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
