
module Runner where


import System.Exit (exitFailure)

import AST         (Tree)
import Generator   (generate)
import Lexer       (tokenize)
import Parser      (parse)
import Tokens      (Token)


lexString :: String -> IO [Token]
lexString s = do
        let lexed = tokenize s
        case lexed of
             (Left err)   -> do
                     print err
                     exitFailure
             (Right toks) -> pure toks


parseTokens :: [Token] -> IO Tree
parseTokens toks = do
        let parsed = parse toks
        case parsed of
             (Left err) -> do
                     print err
                     exitFailure
             (Right ast) -> pure ast


generateASM :: Tree -> IO String
generateASM ast = do
        let generated = generate ast
        case generated of
             (Left err) -> do
                     print err
                     exitFailure
             (Right asm) -> pure asm
