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
        toks <- errorHandler . Lexer.tokenize $ c
        ast  <- errorHandler . Parser.parse $ toks
        ast' <- errorHandler . Checker.check $ ast
        errorHandler . Generator.generate $ ast'
        where
                errorHandler = handleError c


handleError :: String -> Either CompilerError a -> IO a
handleError _ (Right out) = pure out
handleError c (Left err)  = do
        PrintError.printError c err
        exitFailure
