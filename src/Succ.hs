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
        lexed   <- handle . Lexer.tokenize $ c
        parsed  <- handle . Parser.parse $ lexed
        checked <- handle . Checker.check $ parsed
        handle . Generator.generate $ checked
        where
                handle = handler c


handler :: String -> Either CompilerError a -> IO a
handler _ (Right out) = handleSuccess out
handler s (Left err)  = do
        putStr s
        handleError err
        exitFailure


handleSuccess :: a -> IO a
handleSuccess out = pure out


handleError :: CompilerError -> IO ()
handleError err = PrintError.printError err
