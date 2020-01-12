{-|
Module       : Succ
Description  : Run compilation process

Controls the output of the compilation process.
-}
module Succ (compile) where


import           System.Exit (exitFailure)

import           AST         (Tree)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import           Tokens      (Token)


-- | Run the compilation process
compile :: String -> IO String
compile c = tokenize c >>= parse >>= generate


tokenize :: String -> IO [Token]
tokenize s = handle . Lexer.tokenize $ s


parse :: [Token] -> IO Tree
parse toks = handle . Parser.parse $ toks


generate :: Tree -> IO String
generate ast = handle . Generator.generate $ ast


handle :: Either CompilerError a -> IO a
handle (Right out) = handleSuccess out
handle (Left err)  = do
        handleError err
        exitFailure


handleSuccess :: a -> IO a
handleSuccess out = pure out


handleError :: CompilerError -> IO ()
handleError err = print err
