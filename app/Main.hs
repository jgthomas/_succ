
module Main where

import System.Environment (getArgs)
import System.FilePath    (dropExtension)
import System.Process     (system)
import System.Exit        (exitFailure)
import Control.DeepSeq    (deepseq)
import System.IO          (openFile,
                           IOMode(ReadMode),
                           hGetContents,
                           writeFile,
                           hClose)

import Lexer     (tokenize)
--import Parser    (parse)
import NewParser    (parse)
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
        --parsed <- parseTokens lexed
        parsed <- newParseTokens lexed
        asm    <- generateASM parsed

        let outfileName = dropExtension infileName ++ ".s"

        asm `deepseq` writeFile outfileName asm

        system $ "gcc -g " ++ outfileName
                  ++ " -o " ++ dropExtension outfileName
        system $ "rm " ++ outfileName
        hClose handle


lexString :: String -> IO [Token]
lexString s = do
        let lexed = tokenize s
        case lexed of
             (Left err)   -> do
                     print err
                     exitFailure
             (Right toks) -> return toks


--parseTokens :: [Token] -> IO Tree
--parseTokens toks = return $ parse toks


newParseTokens :: [Token] -> IO Tree
newParseTokens toks = do
        let parsed = parse toks
        case parsed of
             (Left err) -> do
                     print err
                     exitFailure
             (Right ast) -> return ast


generateASM :: Tree -> IO String
generateASM ast = do
        let symTab = newSymTab
            Ev act = genASM ast
            (asm, symTab') = act symTab
        --print symTab' -- uncomment to debug
        return asm
