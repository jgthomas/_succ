{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit (exitFailure)

import qualified Checker     (check)
import           Debug       (Debug)
import qualified Debug       (debug)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import qualified PrintError  (printError)


-- | Run the compilation process
compile :: Debug -> String -> IO String
compile debugSet input = do
        toks          <- errorHandler . Lexer.tokenize $ input
        ast           <- errorHandler . Parser.parse $ toks
        ast'          <- errorHandler . Checker.check $ ast
        (asm, symTab) <- errorHandler . Generator.generate $ ast'
        Debug.debug debugSet input toks ast symTab asm
        pure asm
        where
                errorHandler = handleError debugSet input


handleError :: Debug -> String -> Either CompilerError a -> IO a
handleError _ _ (Right out) = pure out
handleError debugSet input (Left err)  = do
        PrintError.printError debugSet input err
        exitFailure
