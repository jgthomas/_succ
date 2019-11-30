
module NewParser (parse) where


import AST       (Tree(..))
import Types     (Type(..))
import Tokens    (Operator(..),
                  Keyword(..),
                  Token(..))
import Error     (CompilerError(ParserError, ImpossibleError),
                  ParserError(..))
import SuccState (CompilerM,
                  getState,
                  putState,
                  throwError,
                  runCompilerM)


startState :: Tree
startState = ProgramNode []


parse :: [Token] -> Either CompilerError Tree
parse toks = runCompilerM parseTokens toks startState


parseTokens :: [Token] -> CompilerM Tree Tree
parseTokens []   = throwError (ParserError NoTokens)
parseTokens toks = parseTopLevelItems toks


parseTopLevelItems :: [Token] -> CompilerM Tree Tree
parseTopLevelItems [] = do
        ast <- getState
        case ast of
             ProgramNode items -> return $ ProgramNode (reverse items)
             _                 -> throwError ImpossibleError
parseTopLevelItems toks = do
        ast <- getState
        case ast of
             ProgramNode items -> parseTopLevelItem toks
             _                 -> throwError ImpossibleError


parseTopLevelItem :: [Token] -> CompilerM Tree Tree
parseTopLevelItem []   = parseTopLevelItems []
parseTopLevelItem toks = do
        putState $ ProgramNode [VarNode "first", VarNode "second"]
        parseTopLevelItems []
