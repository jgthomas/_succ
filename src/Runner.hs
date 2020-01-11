
module Runner (compile) where


import           System.Exit (exitFailure)

import           AST         (Tree)
import           Error       (CompilerError)
import qualified Generator   (generate)
import qualified Lexer       (tokenize)
import qualified Parser      (parse)
import           Tokens      (Token)


-- | Run the compilation process
compile :: String -> IO String
compile c = lexString c >>= parseTokens >>= generateASM


lexString :: String -> IO [Token]
lexString s = handle . Lexer.tokenize $ s


parseTokens :: [Token] -> IO Tree
parseTokens toks = handle . Parser.parse $ toks


generateASM :: Tree -> IO String
generateASM ast = handle . Generator.generate $ ast


handle :: Either CompilerError a -> IO a
handle (Right out) = handleSuccess out
handle (Left err)  = do
        handleError err
        exitFailure


handleSuccess :: a -> IO a
handleSuccess out = pure out


handleError :: CompilerError -> IO ()
handleError err = print err
