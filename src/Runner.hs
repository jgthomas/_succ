
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


-- | Run the lexer stage of compilation
lexString :: String -> IO [Token]
lexString s = handle $ tokenize s


-- | Run the parser stage of compilation
parseTokens :: [Token] -> IO Tree
parseTokens toks = handle $ parse toks


-- | Run the code generation stage of compilation
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
