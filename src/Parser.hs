{-|
Module       : Parser
Description  : Produces an abstract syntax tree

Converts a list of tokens into an abstract syntax tree
representing the C program.
-}
module Parser (parse) where


import           AST              (Tree (..))
import           Error            (CompilerError (ImpossibleError, ParserError, SyntaxError),
                                   ParserError (..), SyntaxError (..))
import           LexDat           (LexDat (..))
import           ParState         (ParserState, runParState, throwError)
import qualified ParState         (getState, putState, startState)
import           Tokens           (Keyword (..), OpTok (..), Token (..))
import qualified Tokens           (isAssign)


import           ParserExpression (parseExpression)
import           ParserShared


-- | Convert a list of tokens into an AST
parse :: [LexDat] -> Either CompilerError Tree
parse lexData = runParState parseTokens lexData ParState.startState


parseTokens :: [LexDat] -> ParserState Tree
parseTokens []      = throwError $ ParserError (LexDataError [])
parseTokens lexData = parseTopLevelItems lexData


parseTopLevelItems :: [LexDat] -> ParserState Tree
parseTopLevelItems [] = ProgramNode . reverse <$> ParState.getState
parseTopLevelItems lexData@(LexDat{tok=Keyword _}:_) = do
        items            <- ParState.getState
        (item, lexData') <- parseTopLevelItem lexData
        ParState.putState $ ProgramNode (item:items)
        parseTopLevelItems lexData'
parseTopLevelItems lexData = throwError $ ParserError (LexDataError lexData)


parseTopLevelItem :: [LexDat] -> ParserState (Tree, [LexDat])
parseTopLevelItem lexData@(_:_:_:LexDat{tok=OpenParen}:_)  = parseFunction lexData
parseTopLevelItem lexData@(_:_:LexDat{tok=OpenParen}:_)    = parseFunction lexData
parseTopLevelItem lexData@(_:LexDat{tok=Ident _}:_)        = parseValueDec lexData
parseTopLevelItem lexData@(_:LexDat{tok=OpTok Asterisk}:_) = parsePointerDec lexData
parseTopLevelItem []      = throwError ImpossibleError
parseTopLevelItem (_:b:_) = throwError $ SyntaxError (NonValidIdentifier b)
parseTopLevelItem lexData = throwError $ ParserError (LexDataError lexData)


parseValueDec :: [LexDat] -> ParserState (Tree, [LexDat])
parseValueDec lexData@(_:LexDat{tok=Ident name}:_) = do
        typ               <- parseType lexData
        lexData'          <- consumeTok lexData
        (tree, lexData'') <- parseOptAssign lexData'
        pure (DeclarationNode name typ tree, lexData'')
parseValueDec (_:c:_:_) = throwError $ SyntaxError (NonValidIdentifier c)
parseValueDec lexData   = throwError $ ParserError (LexDataError lexData)


parsePointerDec :: [LexDat] -> ParserState (Tree, [LexDat])
parsePointerDec lexData@(_:_:LexDat{tok=Ident name}:_) = do
        typ               <- parseType lexData
        lexData'          <- consumeNToks 2 lexData
        (tree, lexData'') <- parseOptAssign lexData'
        pure (PointerNode name typ tree, lexData'')
parsePointerDec (_:_:c:_) = throwError $ SyntaxError (NonValidIdentifier c)
parsePointerDec lexData   = throwError $ ParserError (LexDataError lexData)


parseOptAssign :: [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseOptAssign lexData = do
        (tree, lexData') <- parseOptionalAssign lexData
        lexData''        <- verifyAndConsume SemiColon lexData'
        pure (tree, lexData'')


parseOptionalAssign :: [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseOptionalAssign lexData@(_:d@LexDat{tok=OpTok op}:_)
        | Tokens.isAssign op = do
                (tree, lexData') <- parseExpression lexData
                pure (Just tree, lexData')
        | otherwise = throwError $ SyntaxError (UnexpectedLexDat d)
parseOptionalAssign lexData = do
        lexData' <- consumeTok lexData
        pure (Nothing, lexData')


parseFunction :: [LexDat] -> ParserState (Tree, [LexDat])
parseFunction [] = throwError $ ParserError (LexDataError [])
parseFunction lexData@(a:_) = do
        nodeDat         <- mkDat a
        typ             <- parseType lexData
        name            <- parseFuncName lexData
        (params, lexData') <- parseFuncParams lexData
        (items, lexData'') <- parseFuncBlockItems [] lexData'
        pure (FunctionNode typ name params items nodeDat, lexData'')


parseFuncName :: [LexDat] -> ParserState String
parseFuncName (_:LexDat{tok=Ident name}:_)   = pure name
parseFuncName (_:_:LexDat{tok=Ident name}:_) = pure name
parseFuncName (d:_) = throwError $ SyntaxError (NonValidIdentifier d)
parseFuncName [] = throwError $ ParserError (LexDataError [])


parseFuncParams :: [LexDat] -> ParserState ([Tree], [LexDat])
parseFuncParams (_:LexDat{tok=OpTok Asterisk}:_:rest) = parseParams [] rest
parseFuncParams (_:LexDat{tok=Ident _}:rest)          = parseParams [] rest
parseFuncParams lexData = throwError $ ParserError (LexDataError lexData)


parseParams :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseParams prms lexData = parsePassIn prms lexData parseTheParams


parseTheParams :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseTheParams prms lexData@(LexDat{tok=Keyword _}:_) = do
        (tree, lexData') <- parseParam lexData
        parseParams (tree:prms) lexData'
parseTheParams _ lexData = throwError $ ParserError (LexDataError lexData)


parseParam :: [LexDat] -> ParserState (Tree, [LexDat])
parseParam lexData = do
        typ            <- parseType lexData
        lexData'          <- consumeTok lexData
        (tree, lexData'') <- parseParamValue lexData'
        case tree of
             VarNode _ -> pure (ParamNode typ tree, lexData'')
             _         -> throwError $ ParserError (TreeError tree)


parseParamValue :: [LexDat] -> ParserState (Tree, [LexDat])
parseParamValue (LexDat{tok=OpTok Asterisk}:rest) = parseExpression rest
parseParamValue lexData@(LexDat{tok=Ident _}:_)   = parseExpression lexData
parseParamValue lexData = throwError $ ParserError (LexDataError lexData)


parseFuncBlockItems :: [Tree] -> [LexDat] -> ParserState (Maybe [Tree], [LexDat])
parseFuncBlockItems _ (LexDat{tok=SemiColon}:rest) = pure (Nothing, rest)
parseFuncBlockItems stmts (LexDat{tok=OpenBrace}:rest) = do
        (tree, lexData') <- parseBlock stmts rest
        lexData''        <- verifyAndConsume CloseBrace lexData'
        pure (Just tree, lexData'')
parseFuncBlockItems _ lexData = throwError $ ParserError (LexDataError lexData)


parseBlock :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseBlock stmts lexData@(LexDat{tok=CloseBrace}:_) = pure (reverse stmts, lexData)
parseBlock stmts lexData = do
        (tree, lexData') <- parseBlockItem lexData
        parseBlock (tree:stmts) lexData'


parseBlockItem :: [LexDat] -> ParserState (Tree, [LexDat])
parseBlockItem lexData@(LexDat{tok=Keyword Int}:LexDat{tok=Ident _}:_) =
        parseValueDec lexData
parseBlockItem lexData@(LexDat{tok=Keyword Int}:LexDat{tok=OpTok Asterisk}:_) =
        parsePointerDec lexData
parseBlockItem lexData = parseStatement lexData


parseStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseStatement [] = throwError $ ParserError (LexDataError [])
parseStatement lexData@(first:rest) =
        case first of
             LexDat{tok=Keyword Return}   -> parseReturnStmt rest
             LexDat{tok=Keyword If}       -> parseIfStatement rest
             LexDat{tok=Keyword While}    -> parseWhileStatement rest
             LexDat{tok=Keyword Do}       -> parseDoWhile rest
             LexDat{tok=Keyword For}      -> parseForLoop rest
             LexDat{tok=Keyword Break}    -> parseBreak rest
             LexDat{tok=Keyword Continue} -> parseContinue rest
             LexDat{tok=OpenBrace}        -> parseCompoundStmt rest
             _                            -> parseExprStatement lexData


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
parseExprStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseExprStatement (LexDat{tok=SemiColon}:rest) = parseNullStatement rest
parseExprStatement lexData = do
        (tree, lexData') <- parseExpression lexData
        lexData''        <- verifyAndConsume SemiColon lexData'
        pure (ExprStmtNode tree, lexData'')


parseBreak :: [LexDat] -> ParserState (Tree, [LexDat])
parseBreak (LexDat{tok=SemiColon}:rest) = pure (BreakNode, rest)
parseBreak (d:_) = throwError $ SyntaxError (MissingToken SemiColon d)
parseBreak [] = throwError $ ParserError (LexDataError [])


parseContinue :: [LexDat] -> ParserState (Tree, [LexDat])
parseContinue (LexDat{tok=SemiColon}:rest) = pure (ContinueNode, rest)
parseContinue (d:_) = throwError $ SyntaxError (MissingToken SemiColon d)
parseContinue [] = throwError $ ParserError (LexDataError [])


parseCompoundStmt :: [LexDat] -> ParserState (Tree, [LexDat])
parseCompoundStmt lexData = do
        (items, lexData') <- parseBlock [] lexData
        lexData''         <- verifyAndConsume CloseBrace lexData'
        pure (CompoundStmtNode items, lexData'')


parseForLoop :: [LexDat] -> ParserState (Tree, [LexDat])
parseForLoop lexData = do
        lexData'               <- verifyAndConsume OpenParen lexData
        (ini, lexData'')       <- parseBlockItem lexData'
        (test, lexData''')     <- parseExprStatement lexData''
        (change, lexData'''')  <- parsePostExp lexData'''
        lexData'''''           <- verifyAndConsume CloseParen lexData''''
        (stmts, lexData'''''') <- parseStatement lexData'''''
        if test == NullExprNode
           then pure (ForLoopNode ini (ConstantNode 1) change stmts, lexData'''''')
           else pure (ForLoopNode ini test change stmts, lexData'''''')


parsePostExp :: [LexDat] -> ParserState (Tree, [LexDat])
parsePostExp lexData = do
        (tree, lexData') <- parseForLoopPostExp lexData
        nextTokIsNot SemiColon lexData'
        pure (tree, lexData')


parseForLoopPostExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseForLoopPostExp (d@LexDat{tok=SemiColon}:_) =
        throwError $ SyntaxError (UnexpectedLexDat d)
parseForLoopPostExp lexData@(LexDat{tok=CloseParen}:_) =
        nullExpr lexData
parseForLoopPostExp lexData = parseExpression lexData


parseDoWhile :: [LexDat] -> ParserState (Tree, [LexDat])
parseDoWhile lexData@(LexDat{tok=OpenBrace}:_) = do
        (stmts, lexData') <- parseStatement lexData
        case lexData' of
             (LexDat{tok=Keyword While}:LexDat{tok=OpenParen}:rest) -> do
                     (test, lexData'') <- parseExpression rest
                     lexData'''        <- verifyAndConsume CloseParen lexData''
                     lexData''''       <- verifyAndConsume SemiColon lexData'''
                     pure (DoWhileNode stmts test, lexData'''')
             (_:d@LexDat{tok=OpenParen}:_) ->
                     throwError $ SyntaxError (MissingKeyword While d)
             (d@LexDat{tok=Keyword While}:_:_) ->
                     throwError $ SyntaxError (MissingToken OpenParen d)
             _ -> throwError $ ParserError (LexDataError lexData')
parseDoWhile (d:_) = throwError $ SyntaxError (MissingToken OpenBrace d)
parseDoWhile [] = throwError $ ParserError (LexDataError [])


parseWhileStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseWhileStatement lexData = do
        (test, lexData')   <- parseConditionalParen lexData
        (stmts, lexData'') <- parseStatement lexData'
        pure (WhileNode test stmts, lexData'')


parseIfStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseIfStatement lexData = do
        (test, lexData')       <- parseConditionalParen lexData
        (stmts, lexData'')     <- parseStatement lexData'
        (possElse, lexData''') <- parseOptionalElse lexData''
        pure (IfNode test stmts possElse, lexData''')


parseConditionalParen :: [LexDat] -> ParserState (Tree, [LexDat])
parseConditionalParen lexData = do
        lexData'             <- verifyAndConsume OpenParen lexData
        (test, lexData'')    <- parseExpression lexData'
        lexData'''           <- verifyAndConsume CloseParen lexData''
        pure (test, lexData''')


parseOptionalElse :: [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseOptionalElse (LexDat{tok=Keyword Else}:rest) = do
        (tree, lexData') <- parseStatement rest
        pure (Just tree, lexData')
parseOptionalElse lexData = pure (Nothing, lexData)


parseReturnStmt :: [LexDat] -> ParserState (Tree, [LexDat])
parseReturnStmt lexData = do
        (tree, lexData') <- parseExpression lexData
        lexData''        <- verifyAndConsume SemiColon lexData'
        pure (ReturnNode tree, lexData'')


parseNullStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseNullStatement lexData = pure (NullExprNode, lexData)


nullExpr :: [LexDat] -> ParserState (Tree, [LexDat])
nullExpr lexData = pure (NullExprNode, lexData)
