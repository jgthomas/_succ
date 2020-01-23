{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit (exitFailure)

import qualified Checker     (check)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import qualified PrintError  (printError)


-- | Run the compilation process
compile :: String -> IO String
compile c = do
        toks <- handle . Lexer.tokenize $ c
        ast  <- handle . Parser.parse $ toks
        ast' <- handle . Checker.check $ ast
        handle . Generator.generate $ ast'
        where
                handle = handler c


handler :: String -> Either CompilerError a -> IO a
handler _ (Right out) = pure out
handler c (Left err)  = do
        PrintError.printError c err
        exitFailure
