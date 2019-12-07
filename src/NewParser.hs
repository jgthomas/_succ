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
parseTopLevelItems toks@(TokKeyword typ:rest)
        | validType typ = do
                ast           <- getState
                items         <- getTreeList ast
                (item, toks') <- parseTopLevelItem toks
                putState $ ProgramNode (item:items)
                parseTopLevelItems toks'
        | otherwise = throwError $ TypeError (InvalidType (TokKeyword typ))


parseTopLevelItem :: [Token] -> ParserState (Tree, [Token])
parseTopLevelItem [] = throwError ImpossibleError
parseTopLevelItem toks@(_:_:_:TokOpenParen:_) = parseFunction toks
parseTopLevelItem toks@(_:_:TokOpenParen:_)   = parseFunction toks
parseTopLevelItem toks                        = parseDeclaration toks


parseDeclaration :: [Token] -> ParserState (Tree, [Token])
parseDeclaration [] = throwError ImpossibleError
parseDeclaration toks@(_:TokOp Multiply:_) = parsePointerDec toks
parseDeclaration toks@(_:TokIdent name:_) = do
        varType        <- parseType toks
        toks'          <- consumeTok toks
        (tree, toks'') <- parseOptAssign toks'
        return (DeclarationNode name varType tree, toks'')
parseDeclaration (_:id:_) = throwError $ SyntaxError (InvalidIdentifier id)


parseFunction :: [Token] -> ParserState (Tree, [Token])
parseFunction toks = do
        typ             <- parseType toks
        name            <- parseFuncName toks
        (params, toks') <- parseFuncParams toks
        (items, toks'') <- parseFuncBlockItems [] toks'
        return (FunctionNode typ name params items, toks'')


parseFuncName :: [Token] -> ParserState String
parseFuncName (_:TokIdent name:_)   = return name
parseFuncName (_:_:TokIdent name:_) = return name
parseFuncName toks = throwError $ SyntaxError MissingIdentifier


parseFuncParams :: [Token] -> ParserState ([Tree], [Token])
parseFuncParams (_:TokOp Multiply:_:rest) = parseParams [] rest
parseFuncParams (_:TokIdent name:rest)    = parseParams [] rest


parseParams :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseParams prms toks = parsePassIn prms toks parseTheParams


parseTheParams :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseTheParams prms toks@(TokKeyword typ:_)
        | validType typ = do
                (tree, toks') <- parseParam toks
                parseParams (tree:prms) toks'
        | otherwise = throwError $ TypeError (InvalidType (TokKeyword typ))


parseParam :: [Token] -> ParserState (Tree, [Token])
parseParam toks = do
        typ            <- parseType toks
        toks'          <- consumeTok toks
        (tree, toks'') <- parseParamValue toks'
        case tree of
             VarNode id -> return (ParamNode typ tree, toks'')
             _ -> throwError $ ParserError (TreeError tree)


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
parseBlockItem toks@(TokKeyword kwd:_)
        | validType kwd = parseDeclaration toks
        | otherwise     = parseStatement toks
parseBlockItem toks = parseStatement toks


parseStatement :: [Token] -> ParserState (Tree, [Token])
parseStatement toks@(first:rest) =
        case first of
             TokKeyword Return   -> parseReturnStmt rest
             TokKeyword If       -> parseIfStatement rest
             TokKeyword While    -> parseWhileStatement rest
             TokKeyword Do       -> parseDoWhile rest
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
        toks'               <- verifyAndConsume TokOpenParen toks
        (init, toks'')      <- parseBlockItem toks'
        (test, toks''')     <- parseExprStatement toks''
        (change, toks'''')  <- parsePostExp toks'''
        toks'''''           <- verifyAndConsume TokCloseParen toks''''
        (stmts, toks'''''') <- parseStatement toks'''''
        if test == NullExprNode
           then return (ForLoopNode init (ConstantNode 1) change stmts, toks'''''')
           else return (ForLoopNode init test change stmts, toks'''''')


parsePostExp :: [Token] -> ParserState (Tree, [Token])
parsePostExp toks = do
        (tree, toks') <- parseForLoopPostExp toks
        nextTokIsNot TokSemiColon toks'
        return (tree, toks')


parseForLoopPostExp :: [Token] -> ParserState (Tree, [Token])
parseForLoopPostExp (TokSemiColon:_) =
        throwError $ SyntaxError (UnexpectedToken TokSemiColon)
parseForLoopPostExp toks@(TokCloseParen:_) = nullExpr toks
parseForLoopPostExp toks                   = parseExpression toks


parseDoWhile :: [Token] -> ParserState (Tree, [Token])
parseDoWhile toks@(TokOpenBrace:_) = do
        (stmts, toks') <- parseStatement toks
        case toks' of
             (TokKeyword While:TokOpenParen:rest) -> do
                     (test, toks'') <- parseExpression rest
                     toks'''        <- verifyAndConsume TokCloseParen toks''
                     toks''''       <- verifyAndConsume TokSemiColon toks'''
                     return (DoWhileNode stmts test, toks'''')
             (_:TokOpenParen:_) ->
                     throwError $ SyntaxError (MissingKeyword While)
             (TokKeyword While:_:_) ->
                     throwError $ SyntaxError (MissingToken TokOpenParen)
             _ -> throwError $ ParserError (TokensError toks')
parseDoWhile _ = throwError $ SyntaxError (MissingToken TokOpenBrace)


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
parseOptionalElse (TokKeyword Else:rest) = do
        (tree, toks') <- parseStatement rest
        return (Just tree, toks')
parseOptionalElse toks = return (Nothing, toks)


parseReturnStmt :: [Token] -> ParserState (Tree, [Token])
parseReturnStmt toks = do
        (tree, toks') <- parseExpression toks
        toks''        <- verifyAndConsume TokSemiColon toks'
        return (ReturnNode tree, toks'')


parseNullStatement :: [Token] -> ParserState (Tree, [Token])
parseNullStatement toks = return (NullExprNode, toks)


parsePointerDec :: [Token] -> ParserState (Tree, [Token])
parsePointerDec toks@(_:_:TokIdent name:_) = do
        typ            <- parseType toks
        toks'          <- consumeNToks 2 toks
        (tree, toks'') <- parseOptAssign toks'
        return (PointerNode name typ tree, toks'')
parsePointerDec (_:_:c:_) = throwError $ SyntaxError (InvalidIdentifier c)


parseOptAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptAssign toks = do
        (tree, toks') <- parseOptionalAssign toks
        toks''        <- verifyAndConsume TokSemiColon toks'
        return (tree, toks'')


parseOptionalAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalAssign toks@(id:equ:rest)
        | isAssignment equ = do
                (tree, toks') <- parseExpression toks
                return (Just tree, toks')
        | otherwise = return (Nothing, equ:rest)


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
                        then parseFuncCall toks
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


parseFuncCall :: [Token] -> ParserState (Tree, [Token])
parseFuncCall toks@(TokIdent id:TokOpenParen:_) = do
        toks'          <- consumeTok toks
        (tree, toks'') <- parseArgs [] toks'
        return (FuncCallNode id tree, toks'')
parseFuncCall (TokIdent id:_:_) =
        throwError $ SyntaxError (MissingToken TokOpenParen)
parseFuncCall (a:TokOpenParen:_) =
        throwError $ SyntaxError (InvalidIdentifier a)
parseFuncCall (a:_:_) =
        throwError $ SyntaxError (UnexpectedToken a)
parseFuncCall toks =
        throwError $ ParserError (TokensError toks)


parseArgs :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseArgs args toks = parsePassIn args toks parseTheArgs


parseTheArgs :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseTheArgs as toks = do
        (tree, toks') <- parseExpression toks
        parseArgs (tree:as) toks'


parsePassIn :: [Tree]
            -> [Token]
            -> ([Tree] -> [Token] -> ParserState ([Tree], [Token]))
            -> ParserState ([Tree], [Token])
parsePassIn xs [] _ = throwError $ ParserError (TokensError [])
parsePassIn xs (TokOpenParen:TokCloseParen:rest) _ = return (xs, rest)
parsePassIn xs (TokCloseParen:rest) _              = return (reverse xs, rest)
parsePassIn xs (TokComma:TokCloseParen:_) _ =
        throwError $ SyntaxError (UnexpectedToken TokComma)
parsePassIn xs (TokOpenParen:rest) f = f xs rest
parsePassIn xs (TokComma:rest) f     = f xs rest
parsePassIn xs (a:_) _ = throwError $ SyntaxError (UnexpectedToken a)


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
verifyAndConsume t toks = do
        nextTokIs t toks
        consumeTok toks


nextTokIs :: Token -> [Token] -> ParserState ()
nextTokIs t []    = throwError $ SyntaxError (MissingToken t)
nextTokIs t [a]   = checkIsTok t a
nextTokIs t (a:_) = checkIsTok t a


nextTokIsNot :: Token -> [Token] -> ParserState ()
nextTokIsNot t []    = throwError $ ParserError (TokensError [])
nextTokIsNot t [a]   = checkIsNotTok t a
nextTokIsNot t (a:_) = checkIsNotTok t a


checkIsTok :: Token -> Token -> ParserState ()
checkIsTok t a
        | t == a = return ()
        | otherwise  = throwError $ SyntaxError (MissingToken t)


checkIsNotTok :: Token -> Token -> ParserState ()
checkIsNotTok t a
        | t /= a    = return ()
        | otherwise = throwError $ SyntaxError (UnexpectedToken a)


consumeTok :: [Token] -> ParserState [Token]
consumeTok []       = throwError $ ParserError (TokensError [])
consumeTok [_]      = return []
consumeTok (_:toks) = return toks


consumeNToks :: Int -> [Token] -> ParserState [Token]
consumeNToks 0 toks = return toks
consumeNToks n toks = do
        toks' <- consumeTok toks
        consumeNToks (n-1) toks'


parseType :: [Token] -> ParserState Type
parseType (TokKeyword Int:TokOp Multiply:_) = return IntPointer
parseType (TokKeyword Int:_)                = return IntVar
parseType (a:_) = throwError $ TypeError (InvalidType a)


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
