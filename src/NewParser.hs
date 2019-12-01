
module NewParser (parse) where


import AST       (Tree(..))
import Types     (Type(..))
import Tokens    (Operator(..),
                  Keyword(..),
                  Token(..)
                 )
import Error     (CompilerError(..),
                  ParserError(..),
                  SyntaxError(..),
                  TypeError(..)
                 )
import SuccState (SuccStateM,
                  getState,
                  putState,
                  throwError,
                  runSuccState
                 )


type ParserState = SuccStateM Tree


startState :: Tree
startState = ProgramNode []


parse :: [Token] -> Either CompilerError Tree
parse toks = runSuccState parseTokens toks startState


parseTokens :: [Token] -> ParserState Tree
parseTokens []   = throwError (ParserError NoTokens)
parseTokens toks = parseTopLevelItems toks


parseTopLevelItems :: [Token] -> ParserState Tree
parseTopLevelItems [] = do
        ast <- getState
        case ast of
             ProgramNode items -> return $ ProgramNode (reverse items)
             _                 -> throwError ImpossibleError
parseTopLevelItems toks@(a:rest) =
        case a of
             (TokKeyword typ)
                | validType typ -> do
                        (item, toks') <- parseTopLevelItem toks
                        updateParserState item
                        parseTopLevelItems toks'
                | otherwise -> throwError $ TypeError (InvalidType a)
             _ -> throwError $ TypeError (InvalidType a)


parseTopLevelItem :: [Token] -> ParserState (Tree, [Token])
parseTopLevelItem [] = throwError ImpossibleError
parseTopLevelItem toks
        | isFunction toks = parseFunction toks
        | otherwise       = parseDeclaration toks


parseFunction :: [Token] -> ParserState (Tree, [Token])
parseFunction = undefined


parseDeclaration :: [Token] -> ParserState (Tree, [Token])
parseDeclaration [] = throwError ImpossibleError
parseDeclaration toks@(typ:id:rest) =
        case id of
             (TokIdent varName) -> return (VarNode varName, [])
             _                  -> throwError $ SyntaxError (InvalidIdentifier id)


parseOptAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptAssign toks = parseOptionalAssign toks


parseOptionalAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalAssign = undefined


updateParserState :: Tree -> ParserState ()
updateParserState tree = do
        ast      <- getState
        treeList <- getTreeList ast
        putState $ ProgramNode (tree:treeList)


getTreeList :: Tree -> ParserState [Tree]
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


validType :: Keyword -> Bool
validType kwd = kwd == Int
