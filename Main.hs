
module Main (main) where


import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)
import Control.Monad (when)

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

        when (length assembly > 0) $
           writeFile outfileName assembly

        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
