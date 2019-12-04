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
parseTopLevelItems toks@(TokKeyword typ:rest) =
        if validType typ
           then do
                   ast           <- getState
                   items         <- getTreeList ast
                   (item, toks') <- parseTopLevelItem toks
                   putState $ ProgramNode (item:items)
                   parseTopLevelItems toks'
           else throwError $ TypeError (InvalidType (TokKeyword typ))


parseTopLevelItem :: [Token] -> ParserState (Tree, [Token])
parseTopLevelItem [] = throwError ImpossibleError
parseTopLevelItem toks
        | isFunction toks = parseFunction toks
        | otherwise       = parseDeclaration toks


parseFunction :: [Token] -> ParserState (Tree, [Token])
parseFunction toks@(a:b:c:rest) = do
        typ             <- setType a b
        name            <- parseFuncName b c
        (params, toks') <- parseFuncParams b c rest
        (items, toks'') <- parseFuncBlockItems [] toks'
        return (FunctionNode typ name params items, toks'')


parseFuncName :: Token -> Token -> ParserState String
parseFuncName (TokOp Multiply) (TokIdent name) = return name
parseFuncName (TokIdent name)  _               = return name
parseFuncName _                _               = throwError $ SyntaxError MissingIdentifier


parseFuncParams :: Token -> Token -> [Token] -> ParserState ([Tree], [Token])
parseFuncParams (TokOp Multiply) (TokIdent name) toks = parseParams [] toks
parseFuncParams (TokIdent name) tok toks              = parseParams [] (tok:toks)


parseParams :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseParams paramList (TokOpenParen:TokCloseParen:rest)  = return (reverse paramList, rest)
parseParams paramList (TokCloseParen:rest)               = return (reverse paramList, rest)
parseParams paramList (TokComma:TokCloseParen:rest)      = throwError $ SyntaxError (UnexpectedToken TokComma)
parseParams paramList (TokOpenParen:TokKeyword typ:rest) = parseParams2 paramList (TokKeyword typ:rest)
parseParams paramList (TokComma:TokKeyword typ:rest)     = parseParams2 paramList (TokKeyword typ:rest)


parseParams2 :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseParams2 paramList toks@(TokKeyword typ:rest) =
        if validType typ
           then do
                   (tree, toks') <- parseParam toks
                   parseParams (tree:paramList) toks'
           else throwError $ TypeError (InvalidType (TokKeyword typ))


parseParam :: [Token] -> ParserState (Tree, [Token])
parseParam toks@(a:b:rest) = do
        toks'          <- verifyAndConsume a toks
        (tree, toks'') <- parseParamValue toks'
        typ            <- setType a b
        case tree of
             VarNode id -> return (ParamNode typ tree, toks'')
             _          -> throwError $ ParserError (ParseError "Invalid parameter")


parseParamValue :: [Token] -> ParserState (Tree, [Token])
parseParamValue toks@(TokOp Multiply:rest) = parseExpression rest
parseParamValue toks@(TokIdent id:rest)    = parseExpression toks


parseFuncBlockItems :: [Tree] -> [Token] -> ParserState (Maybe [Tree], [Token])
parseFuncBlockItems stmts (TokSemiColon:rest) = return (Nothing, rest)
parseFuncBlockItems stmts (TokOpenBrace:rest) = do
        (tree, toks') <- parseBlock stmts rest
        toks''        <- verifyAndConsume TokCloseBrace toks'
        return (Just tree, toks'')


parseBlock :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseBlock stmts toks@(TokCloseBrace:_) = return (reverse stmts, toks)
parseBlock stmts toks = do
        (tree, toks') <- parseBlockItem toks
        parseBlock (tree:stmts) toks'


parseBlockItem :: [Token] -> ParserState (Tree, [Token])
parseBlockItem toks@(a:rest) =
        case a of
             (TokKeyword kwd) | validType kwd -> parseDeclaration toks
             _                                -> parseStatement toks


parseStatement :: [Token] -> ParserState (Tree, [Token])
parseStatement toks@(first:rest) =
        case first of
             TokKeyword Return   -> parseReturnStmt rest
             TokKeyword If       -> parseIfStatement rest
             TokKeyword While    -> parseWhileStatement rest
             TokKeyword Do       -> parseDoWhileStatement rest
             TokKeyword For      -> parseForLoop rest
             TokKeyword Break    -> parseBreak rest
             TokKeyword Continue -> parseContinue rest
             TokOpenBrace        -> parseCompoundStmt rest
             _                   -> parseExprStatement toks


{-
- Parses expressions where a semi-colon is required afterwards
-
- null expression:         ;
- expression statements:   2 + 2;
- elements of loops:       (i = 0; i < 10; i++)
- assignments:             a = 10; *p = 10;
- function calls:          dog(8);
-
-}
parseExprStatement :: [Token] -> ParserState (Tree, [Token])
parseExprStatement (TokSemiColon:rest) = parseNullStatement rest
parseExprStatement toks = do
        (tree, toks') <- parseExpression toks
        toks''        <- verifyAndConsume TokSemiColon toks'
        return (ExprStmtNode tree, toks'')


parseBreak :: [Token] -> ParserState (Tree, [Token])
parseBreak toks = do
        toks' <- verifyAndConsume TokSemiColon toks
        return (BreakNode, toks')


parseContinue :: [Token] -> ParserState (Tree, [Token])
parseContinue toks = do
        toks' <- verifyAndConsume TokSemiColon toks
        return (ContinueNode, toks')


parseCompoundStmt :: [Token] -> ParserState (Tree, [Token])
parseCompoundStmt toks = do
        (items, toks') <- parseBlock [] toks
        toks''         <- verifyAndConsume TokCloseBrace toks'
        return (CompoundStmtNode items, toks'')


parseForLoop :: [Token] -> ParserState (Tree, [Token])
parseForLoop toks = do
        toks'              <- verifyAndConsume TokOpenParen toks
        (init, toks'')     <- parseBlockItem toks'
        (test, toks''')    <- parseExprStatement toks''
        (change, toks'''') <- parseForLoopPostExp toks'''
        if lookAhead toks'''' == TokSemiColon
           then throwError $ SyntaxError (UnexpectedToken TokSemiColon)
           else do
                   toks'''''           <- verifyAndConsume TokCloseParen toks''''
                   (stmts, toks'''''') <- parseStatement toks'''''
                   if test == NullExprNode
                      then return (ForLoopNode init (ConstantNode 1) change stmts, toks'''''')
                      else return (ForLoopNode init test change stmts, toks'''''')


parseForLoopPostExp :: [Token] -> ParserState (Tree, [Token])
parseForLoopPostExp (TokSemiColon:_)       = throwError $ SyntaxError (UnexpectedToken TokSemiColon)
parseForLoopPostExp toks@(TokCloseParen:_) = nullExpr toks
parseForLoopPostExp toks                   = parseExpression toks


parseDoWhileStatement :: [Token] -> ParserState (Tree, [Token])
parseDoWhileStatement toks = do
        toks'           <- verifyAndConsume TokOpenBrace toks
        (stmts, toks'') <- parseStatement toks
        case toks'' of
             (a:b:rest)
                | a /= TokKeyword While -> throwError $ SyntaxError (MissingKeyword While)
                | b /= TokOpenParen     -> throwError $ SyntaxError (MissingToken TokOpenParen)
                | otherwise -> do
                        (test, toks''') <- parseExpression rest
                        toks''''        <- verifyAndConsume TokCloseParen toks'''
                        toks'''''       <- verifyAndConsume TokSemiColon toks''''
                        return (DoWhileNode stmts test, toks''''')


parseWhileStatement :: [Token] -> ParserState (Tree, [Token])
parseWhileStatement toks = do
        (test, toks')   <- parseConditionalParen toks
        (stmts, toks'') <- parseStatement toks'
        return (WhileNode test stmts, toks'')


parseIfStatement :: [Token] -> ParserState (Tree, [Token])
parseIfStatement toks = do
        (test, toks')       <- parseConditionalParen toks
        (stmts, toks'')     <- parseStatement toks'
        (possElse, toks''') <- parseOptionalElse toks''
        return (IfNode test stmts possElse, toks''')



parseConditionalParen :: [Token] -> ParserState (Tree, [Token])
parseConditionalParen toks = do
        toks'             <- verifyAndConsume TokOpenParen toks
        (test, toks'')    <- parseExpression toks'
        toks'''           <- verifyAndConsume TokCloseParen toks''
        return (test, toks''')


parseOptionalElse :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalElse toks@(a:rest) =
        if a == TokKeyword Else
           then do
                   (tree, toks') <- parseStatement rest
                   return (Just tree, toks')
           else return (Nothing, toks)


parseReturnStmt :: [Token] -> ParserState (Tree, [Token])
parseReturnStmt toks = do
        (tree, toks') <- parseExpression toks
        toks''        <- verifyAndConsume TokSemiColon toks'
        return (ReturnNode tree, toks'')


parseNullStatement :: [Token] -> ParserState (Tree, [Token])
parseNullStatement toks = return (NullExprNode, toks)


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
parsePointerDec toks@(a:b:c:rest) =
        case c of
             (TokIdent varName) -> do
                     (tree, toks') <- parseOptAssign (c:rest)
                     typ           <- setType a b
                     return (PointerNode varName typ tree, toks')
             _ -> throwError $ SyntaxError (InvalidIdentifier c)


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
                   toks''             <- verifyAndConsume next toks'
                   (subTree, toks''') <- parseExpression toks''
                   opVal              <- opValue next
                   case tree of
                     (VarNode id) ->
                             return (AssignmentNode id subTree opVal, toks''')
                     (DereferenceNode id) ->
                             return (AssignDereferenceNode id subTree opVal, toks''')
                     _ -> throwError $ ParserError (ParseError "a")
           else return (tree, toks')


parseTernaryExp :: [Token] -> ParserState (Tree, [Token])
parseTernaryExp toks = do
        (cond, toks') <- parseLogicalOrExp toks
        case lookAhead toks' of
             TokQuestMark -> do
                     toks''             <- verifyAndConsume TokQuestMark toks'
                     (expr1, toks''')   <- parseExpression toks''
                     toks''''           <- verifyAndConsume TokColon toks'''
                     (expr2, toks''''') <- parseTernaryExp toks''''
                     return (TernaryNode cond expr1 expr2, toks''''')
             _ -> return (cond, toks')


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
                     if lookAhead rest == TokOpenParen
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
parseAddressOf (TokIdent n:rest) = return (AddressOfNode n, rest)
parseAddressOf (a:rest)          = throwError $ SyntaxError (InvalidIdentifier a)


parseDereference :: [Token] -> ParserState (Tree, [Token])
parseDereference (TokIdent n:rest) = return (DereferenceNode n, rest)
parseDereference (a:rest)          = throwError $ SyntaxError (InvalidIdentifier a)


parseFunctionCall :: [Token] -> ParserState (Tree, [Token])
parseFunctionCall (TokIdent id:TokOpenParen:rest) = do
        (tree, toks') <- parseFunctionArgs [] (TokOpenParen:rest)
        return (FuncCallNode id tree, toks')
parseFunctionCall (TokIdent id:b:rest)  = throwError $ SyntaxError (MissingToken TokOpenParen)
parseFunctionCall (a:TokOpenParen:rest) = throwError $ SyntaxError (InvalidIdentifier a)
parseFunctionCall (a:b:rest)            = throwError $ SyntaxError (UnexpectedToken a)


parseFunctionArgs :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseFunctionArgs argList (a:b:rest)
        | a == TokCloseParen                  = return (reverse argList, b:rest)
        | a /= TokOpenParen && a /= TokComma  = throwError $ SyntaxError (MissingToken TokComma)
        | a == TokComma && b == TokCloseParen = throwError $ SyntaxError (UnexpectedToken b)
        | otherwise = if b == TokCloseParen
                         then return (reverse argList, rest)
                         else do (tree, toks') <- parseArgument (b:rest)
                                 parseFunctionArgs (tree:argList) toks'


parseArgument :: [Token] -> ParserState (Tree, [Token])
parseArgument toks = do
        (tree, toks') <- parseExpression toks
        return (ArgNode tree, toks')


parseBinaryExp :: Tree
               -> [Token]
               -> ([Token] -> ParserState (Tree, [Token]))
               -> [Operator]
               -> ParserState (Tree, [Token])
parseBinaryExp tree toks f ops = do
        let next = lookAhead toks
        case next of
             TokOp op | op `elem` ops -> do
                     toks'           <- verifyAndConsume next toks
                     (ntree, toks'') <- f toks'
                     parseBinaryExp (BinaryNode tree ntree op) toks'' f ops
             _ -> return (tree, toks)


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
verifyAndConsume t (a:rest)
        | t == a    = return rest
        | otherwise = throwError $ SyntaxError (MissingToken t)


setType :: Token -> Token -> ParserState Type
setType (TokKeyword Int) (TokOp Multiply) = return IntPointer
setType (TokKeyword Int) _                = return IntVar
setType a                b                = throwError $ TypeError (InvalidType a)


nullExpr :: [Token] -> ParserState (Tree, [Token])
nullExpr toks = return (NullExprNode, toks)


opValue :: Token -> ParserState Operator
opValue (TokOp v) = return v
opValue t         = error $ show t


validType :: Keyword -> Bool
validType kwd = kwd == Int


lookAhead :: [Token] -> Token
lookAhead [] = TokWut
lookAhead (c:cs) = c
