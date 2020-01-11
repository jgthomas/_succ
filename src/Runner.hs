
module Runner
        (lexString,
         parseTokens,
         generateASM
        ) where


import System.Exit (exitFailure)

import AST         (Tree)
import Error       (CompilerError)
import Generator   (generate)
import Lexer       (tokenize)
import Parser      (parse)
import Tokens      (Token)


lexString :: String -> IO [Token]
lexString s = handle $ tokenize s


parseTokens :: [Token] -> IO Tree
parseTokens toks = handle $ parse toks


generateASM :: Tree -> IO String
generateASM ast = handle $ generate ast


handle :: Either CompilerError a -> IO a
handle (Right out) = handleSuccess out
handle (Left err)  = do
        handleError err
        exitFailure


handleSuccess :: a -> IO a
handleSuccess out = pure out


handleError :: CompilerError -> IO ()
handleError err = print err
