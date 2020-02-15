{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit (exitFailure)

import           AST         (Tree)
import qualified Checker     (check)
import           Debug       (Debug (..))
import           Error       (CompilerError)
import qualified Generator   (generate)
import           LexDat      (LexDat)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import qualified PrintError  (printError)


-- | Run the compilation process
compile :: Debug -> String -> IO String
compile debugSet input = do
        toks <- errorHandler . Lexer.tokenize $ input
        ast  <- errorHandler . Parser.parse $ toks
        ast' <- errorHandler . Checker.check $ ast
        case debugSet of
             DebugOff -> errorHandler . Generator.generate $ ast'
             DebugOn  -> do
                     debug input toks ast
                     errorHandler . Generator.generate $ ast'
        where errorHandler = handleError input


debug :: String -> [LexDat] -> Tree -> IO ()
debug input lexed parsed = do
        newLine
        putStrLn "INPUT"
        putStrLn input
        putStrLn "AFTER LEXING"
        newLine
        print lexed
        newLine
        putStrLn "AFTER PARSING"
        newLine
        print parsed
        newLine


newLine :: IO ()
newLine = putStr "\n"


handleError :: String -> Either CompilerError a -> IO a
handleError _ (Right out) = pure out
handleError input (Left err)  = do
        PrintError.printError input err
        exitFailure
