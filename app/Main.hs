
module Main where

import System.Environment (getArgs)
import System.FilePath    (dropExtension)
import System.Process     (system)
import Control.Monad      (when)
import System.IO          (openFile,
                           IOMode(ReadMode),
                           hGetContents,
                           writeFile,
                           hClose)

--import Lexer     (tokenize)
--import Parser    (parse)
--import Generator (genASM)
--import Evaluator (Evaluator(Ev))
--import SymTab    (newSymTab)

import NewLexer  (tokenize)


main :: IO()
main = do
        args <- getArgs
        let infileName = head args
        handle   <- openFile infileName ReadMode
        contents <- hGetContents handle

        -- uncomment to debug
        print contents
        let tokens = tokenize contents
        print tokens
        --print $ parse $ tokenize contents

        --let outfileName = dropExtension infileName ++ ".s"
        --    parsed      = parse $ tokenize contents

        --let symTab = newSymTab
        --    Ev act = genASM parsed
        --    (asm, symTab') = act symTab
        --    in do
        --            -- uncomment to debug
        --            --print symTab'
        --            when (length asm > 0) $
        --               writeFile outfileName asm

        --system $ "gcc -g " ++ outfileName
        --          ++ " -o " ++ dropExtension outfileName
        --system $ "rm " ++ outfileName
        --hClose handle
