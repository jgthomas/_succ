
module Main where

import System.Environment (getArgs)
import System.FilePath    (dropExtension)
import System.Process     (system)
import System.Exit        (exitFailure)
import Control.Monad      (when)
import System.IO          (openFile,
                           IOMode(ReadMode),
                           hGetContents,
                           writeFile,
                           hClose)

import Lexer     (tokenize)
import Parser    (parse)
import Generator (genASM)
import Evaluator (Evaluator(Ev))
import SymTab    (newSymTab)
import Tokens    (Token)
import AST       (Tree)


main :: IO()
main = do
        args <- getArgs
        let infileName = head args
        handle   <- openFile infileName ReadMode
        contents <- hGetContents handle

        lexed  <- lexString contents
        parsed <- parseTokens lexed
        asm    <- generateASM parsed

        let outfileName = dropExtension infileName ++ ".s"

        when (length asm > 0) $
           writeFile outfileName asm

        system $ "gcc -g " ++ outfileName
                  ++ " -o " ++ dropExtension outfileName
        system $ "rm " ++ outfileName
        hClose handle


lexString :: [Char] -> IO [Token]
lexString s = do
        let lexed = tokenize s
        case lexed of
             (Left err)   -> do
                     print err
                     exitFailure
             (Right toks) -> return toks


parseTokens :: [Token] -> IO Tree
parseTokens toks = return $ parse toks


generateASM :: Tree -> IO String
generateASM ast = do
        let symTab = newSymTab
            Ev act = genASM ast
            (asm, symTab') = act symTab
        --print symTab' -- uncomment to debug
        return asm
