
module NewParser (parse) where


import AST       (Tree(..))
import Types     (Type(..))
import Tokens    (Operator(..),
                  Keyword(..),
                  Token(..)
                 )
import Error     (CompilerError(ParserError, SyntaxError, ImpossibleError),
                  ParserError(..),
                  SyntaxError(..)
                 )
import SuccState (CompilerM,
                  getState,
                  putState,
                  throwError,
                  runCompilerM
                 )


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
parseTopLevelItem [] = throwError ImpossibleError
parseTopLevelItem toks
        | isFunction toks = parseFunction toks
        | otherwise       = parseDeclaration toks


parseFunction :: [Token] -> CompilerM Tree Tree
parseFunction = undefined


parseDeclaration :: [Token] -> CompilerM Tree Tree
parseDeclaration [] = throwError ImpossibleError
parseDeclaration toks@(typ:id:rest) =
        case id of
             (TokIdent varName) -> updateParserState (VarNode varName) []
             _                  -> throwError $ SyntaxError (InvalidIdentifier id)


parseOptAssign :: [Token] -> CompilerM Tree ([Token], Maybe Tree)
parseOptAssign toks = parseOptionalAssign toks


parseOptionalAssign :: [Token] -> CompilerM Tree ([Token], Maybe Tree)
parseOptionalAssign = undefined


updateParserState :: Tree -> [Token] -> CompilerM Tree Tree
updateParserState tree toks = do
        ast      <- getState
        treeList <- getTreeList ast
        putState $ ProgramNode (tree:treeList)
        parseTopLevelItems toks


getTreeList :: Tree -> CompilerM Tree [Tree]
getTreeList (ProgramNode treeList) = return treeList
getTreeList _                      = throwError ImpossibleError


isFunction :: [Token] -> Bool
isFunction []                  = False
isFunction [_]                 = False
isFunction [_, _]              = False
isFunction [_, _, _]           = False
isFunction toks@(a:b:c:d:rest) = isFuncStart a b c d


isFuncStart :: Token -> Token -> Token -> Token -> Bool
isFuncStart (TokKeyword Int) (TokOp Multiply) (TokIdent id) TokOpenParen = True
isFuncStart (TokKeyword Int) (TokIdent id)    TokOpenParen  _            = True
isFuncStart _                _                _             _            = False


isAssignment :: Operator -> Bool
isAssignment op = op `elem` [Assign,PlusAssign,MinusAssign,
                             MultiplyAssign,DivideAssign,ModuloAssign]
