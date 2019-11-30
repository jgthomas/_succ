
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
parseTopLevelItem []   = throwError ImpossibleError
parseTopLevelItem toks = do updateParserState (VarNode "yes") []


updateParserState :: Tree -> [Token] -> CompilerM Tree Tree
updateParserState tree toks = do
        ast      <- getState
        treeList <- getTreeList ast
        putState $ ProgramNode (tree:treeList)
        parseTopLevelItems toks


getTreeList :: Tree -> CompilerM Tree [Tree]
getTreeList (ProgramNode treeList) = return treeList
getTreeList _                      = throwError ImpossibleError


--parseDeclaration :: [Token] -> CompilerM Tree Tree
--parseDeclaration []   = throwError ImpossibleError
--parseDeclaration toks@(typ:id:rest) = do
--
--
--isFunction :: Token -> Token -> Token -> Token -> Bool
--isFunction (TokKeyword Int) (TokOp Multiply) (TokIdent id) TokOpenParen = True
--isFunction (TokKeyword Int) (TokIdent id)    TokOpenParen  _            = True
--isFunction _                _                _             _            = False
