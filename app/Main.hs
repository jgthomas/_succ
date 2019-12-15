
module Main where

import Control.DeepSeq    (deepseq)
import System.Environment (getArgs)
import System.Exit        (exitFailure)
import System.FilePath    (dropExtension)
import System.IO          (IOMode (ReadMode), hClose, hGetContents, openFile,
                           writeFile)
import System.Process     (system)

import AST                (Tree)
import Generator          (generate)
import Lexer              (tokenize)
import Parser             (parse)
import Tokens             (Token)


main :: IO()
main = do
        args <- getArgs
        let infileName = head args
        handle   <- openFile infileName ReadMode
        contents <- hGetContents handle

        lexed  <- lexString contents
        parsed <- newParseTokens lexed
        asm    <- generateASM parsed

        let outfileName = dropExtension infileName ++ ".s"

        asm `deepseq` writeFile outfileName asm

        let gccOpts = "gcc -g "
            output  = " -o " ++ dropExtension outfileName
            toMachineCode = gccOpts ++ outfileName ++ output
            deleteFile    = "rm " ++ outfileName

        _ <- system toMachineCode
        _ <- system deleteFile
        hClose handle


lexString :: String -> IO [Token]
lexString s = do
        let lexed = tokenize s
        case lexed of
             (Left err)   -> do
                     print err
                     exitFailure
             (Right toks) -> return toks


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
        let generated = generate ast
        case generated of
             (Left err) -> do
                     print err
                     exitFailure
             (Right asm) -> return asm
