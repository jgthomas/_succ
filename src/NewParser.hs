
module NewParser (parse) where


import Tokens (Operator(..), Keyword(..), Token(..))
import qualified Tokens (unary)
import AST       (Tree(..))
import Types     (Type(..))
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
             (TokOp Multiply)   -> parsePointerDec toks
             (TokIdent varName) -> do
                     varType        <- setType typ id
                     toks'          <- verifyAndConsume typ toks
                     (tree, toks'') <- parseOptAssign toks'
                     return (DeclarationNode varName varType tree, toks'')
             _ -> throwError $ SyntaxError (InvalidIdentifier id)


parsePointerDec :: [Token] -> ParserState (Tree, [Token])
parsePointerDec toks = undefined


parseOptAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptAssign toks = do
        (tree, toks') <- parseOptionalAssign toks
        toks''        <- verifyAndConsume TokSemiColon toks'
        return (tree, toks'')


parseOptionalAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalAssign toks@(id:equ:rest) =
        if isAssignment equ
           then do (tree, toks') <- parseExpression toks
                   return (Just tree, toks')
           else return (Nothing, equ:rest)


parseExpression :: [Token] -> ParserState (Tree, [Token])
parseExpression toks = do
        (tree, toks') <- parseTernaryExp toks
        let next = lookAhead toks'
        if isAssignment next
           then do
                   toks'' <- verifyAndConsume next toks'
                   (subTree, toks''') <- parseExpression toks''
                   opVal <- opValue next
                   case tree of
                     (VarNode id) ->
                             return (AssignmentNode id subTree opVal, toks''')
                     (DereferenceNode id) ->
                             return (AssignDereferenceNode id subTree opVal, toks''')
                     _ -> throwError $ ParserError (ParseError "a")
           else return (tree, toks')


parseTernaryExp :: [Token] -> ParserState (Tree, [Token])
parseTernaryExp toks = do
        (cond, toks')      <- parseLogicalOrExp toks
        toks''             <- verifyAndConsume TokQuestMark toks'
        (expr1, toks''')   <- parseExpression toks''
        toks''''           <- verifyAndConsume TokColon toks'''
        (expr2, toks''''') <- parseTernaryExp toks''''
        return (TernaryNode cond expr1 expr2, toks''''')


parseLogicalOrExp :: [Token] -> ParserState (Tree, [Token])
parseLogicalOrExp toks = do
        (orTree, toks') <- parseLogicalAndExp toks
        parseBinaryExp orTree toks' parseLogicalAndExp [LogicalOR]


parseLogicalAndExp :: [Token] -> ParserState (Tree, [Token])
parseLogicalAndExp toks = do
        (andTree, toks') <- parseEqualityExp toks
        parseBinaryExp andTree toks' parseEqualityExp [LogicalAND]


parseEqualityExp :: [Token] -> ParserState (Tree, [Token])
parseEqualityExp toks = do
        (equTree, toks') <- parseRelationalExp toks
        parseBinaryExp equTree toks' parseRelationalExp [Equal,NotEqual]


parseRelationalExp :: [Token] -> ParserState (Tree, [Token])
parseRelationalExp toks = do
        (relaTree, toks') <- parseAdditiveExp toks
        parseBinaryExp relaTree toks' parseAdditiveExp
             [GreaterThan,LessThan,GreaterThanOrEqual,LessThanOrEqual]


parseAdditiveExp :: [Token] -> ParserState (Tree, [Token])
parseAdditiveExp toks = do
        (termTree, toks') <- parseTerm toks
        parseBinaryExp termTree toks' parseTerm [Plus,Minus]


parseTerm :: [Token] -> ParserState (Tree, [Token])
parseTerm toks = do
        (facTree, toks') <- parseFactor toks
        parseBinaryExp facTree toks' parseFactor [Multiply,Divide,Modulo]


parseFactor :: [Token] -> ParserState (Tree, [Token])
parseFactor toks@(next:rest) =
        case next of
             TokSemiColon    -> return (NullExprNode, rest)
             (TokConstInt n) -> return (ConstantNode n, rest)
             (TokIdent id)   ->
                     if lookAhead toks == TokOpenParen
                        then parseFunctionCall toks
                        else return (VarNode id, rest)
             (TokOp op)
                | op == Ampersand -> parseAddressOf rest
                | op == Multiply  -> parseDereference rest
                | op `elem` Tokens.unary -> do
                        (tree, toks') <- parseFactor rest
                        return (UnaryNode tree op, toks')
             TokOpenParen -> do
                     (tree, toks') <- parseExpression rest
                     toks''        <- verifyAndConsume TokCloseParen toks'
                     return (tree, toks'')
             _ -> throwError $ ParserError (ParseError (show toks))


parseAddressOf :: [Token] -> ParserState (Tree, [Token])
parseAddressOf toks = undefined


parseDereference :: [Token] -> ParserState (Tree, [Token])
parseDereference toks = undefined


parseFunctionCall :: [Token] -> ParserState (Tree, [Token])
parseFunctionCall toks@(id:paren:rest) = undefined


parseBinaryExp :: Tree
               -> [Token]
               -> ([Token] -> ParserState (Tree, [Token]))
               -> [Operator]
               -> ParserState (Tree, [Token])
parseBinaryExp tree toks f ops = do
        let next = lookAhead toks
        op <- opValue next
        if op `elem` ops
           then do
                   toks'           <- verifyAndConsume next toks
                   (ntree, toks'') <- f toks'
                   parseBinaryExp (BinaryNode tree ntree op) toks'' f ops
           else return (tree, toks)


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


isAssignment :: Token -> Bool
isAssignment (TokOp op) = op `elem` assignmentToks
isAssignment _          = False


assignmentToks :: [Operator]
assignmentToks = [Assign,
                  PlusAssign,
                  MinusAssign,
                  MultiplyAssign,
                  DivideAssign,
                  ModuloAssign
                 ]


verifyAndConsume :: Token -> [Token] -> ParserState [Token]
verifyAndConsume t [] = throwError $ SyntaxError (MissingToken t)
verifyAndConsume t (a:rest) =
        if t == a
           then return rest
           else throwError $ SyntaxError (MissingToken t)


setType :: Token -> Token -> ParserState Type
setType (TokKeyword Int) (TokOp Multiply) = return IntPointer
setType (TokKeyword Int) _                = return IntVar
setType a                b                = throwError $ TypeError (InvalidType a)


opValue :: Token -> ParserState Operator
opValue (TokOp v) = return v
opValue t         = throwError $ SyntaxError (UnexpectedToken t)


validType :: Keyword -> Bool
validType kwd = kwd == Int


lookAhead :: [Token] -> Token
lookAhead [] = TokWut
lookAhead (c:cs) = c
