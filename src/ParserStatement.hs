
module ParserStatement (parseStatementBlock) where


import AST               (Tree (..))
import Error             (CompilerError (ParserError, SyntaxError),
                          ParserError (..), SyntaxError (..))
import LexDat            (LexDat (..))
import ParserDeclaration (parsePointerDec, parseValueDec)
import ParserExpression  (parseExpression)
import ParserShared      (makeNodeDat, nextTokIsNot, verifyAndConsume)
import ParState          (ParserState, throwError)
import Tokens            (Keyword (..), OpTok (..), Token (..))


parseStatementBlock :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseStatementBlock stmts lexData@(LexDat{tok=CloseBrace}:_) = pure (reverse stmts, lexData)
parseStatementBlock stmts lexData = do
        (tree, lexData') <- parseBlockItem lexData
        parseStatementBlock (tree:stmts) lexData'


parseBlockItem :: [LexDat] -> ParserState (Tree, [LexDat])
parseBlockItem lexData@(LexDat{tok=Keyword Int}:LexDat{tok=Ident _}:_) =
        parseValueDec lexData
parseBlockItem lexData@(LexDat{tok=Keyword Int}:LexDat{tok=OpTok Asterisk}:_) =
        parsePointerDec lexData
parseBlockItem lexData = parseStatement lexData


parseStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseStatement [] = throwError $ ParserError (LexDataError [])
parseStatement lexData@(first:_) =
        case first of
             LexDat{tok=Keyword Return}   -> parseReturnStmt lexData
             LexDat{tok=Keyword If}       -> parseIfStatement lexData
             LexDat{tok=Keyword While}    -> parseWhileStatement lexData
             LexDat{tok=Keyword Do}       -> parseDoWhile lexData
             LexDat{tok=Keyword For}      -> parseForLoop lexData
             LexDat{tok=Keyword Break}    -> parseBreak lexData
             LexDat{tok=Keyword Continue} -> parseContinue lexData
             LexDat{tok=OpenBrace}        -> parseCompoundStmt lexData
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
parseExprStatement lexData@(LexDat{tok=SemiColon}:_) = parseNullStatement lexData
parseExprStatement lexData = do
        dat              <- makeNodeDat lexData
        (tree, lexData') <- parseExpression lexData
        lexData''        <- verifyAndConsume SemiColon lexData'
        pure (ExprStmtNode tree dat, lexData'')


parseBreak :: [LexDat] -> ParserState (Tree, [LexDat])
parseBreak lexData@(LexDat{tok=Keyword Break}:LexDat{tok=SemiColon}:rest) = do
        dat <- makeNodeDat lexData
        pure (BreakNode dat, rest)
parseBreak (LexDat{tok=Keyword Break}:d:_) =
        throwError $ SyntaxError (MissingToken SemiColon d)
parseBreak lexData = throwError $ ParserError (LexDataError lexData)


parseContinue :: [LexDat] -> ParserState (Tree, [LexDat])
parseContinue lexData@(LexDat{tok=Keyword Continue}:LexDat{tok=SemiColon}:rest) = do
        dat <- makeNodeDat lexData
        pure (ContinueNode dat, rest)
parseContinue (LexDat{tok=Keyword Continue}:d:_) =
        throwError $ SyntaxError (MissingToken SemiColon d)
parseContinue lexData = throwError $ ParserError (LexDataError lexData)


parseCompoundStmt :: [LexDat] -> ParserState (Tree, [LexDat])
parseCompoundStmt lexData = do
        dat                <- makeNodeDat lexData
        lexData'           <- verifyAndConsume OpenBrace lexData
        (items, lexData'') <- parseStatementBlock [] lexData'
        lexData'''         <- verifyAndConsume CloseBrace lexData''
        pure (CompoundStmtNode items dat, lexData''')


parseForLoop :: [LexDat] -> ParserState (Tree, [LexDat])
parseForLoop lexData = do
        dat                     <- makeNodeDat lexData
        lexData'                <- verifyAndConsume (Keyword For) lexData
        lexData''               <- verifyAndConsume OpenParen lexData'
        (ini, lexData''')       <- parseBlockItem lexData''
        (test, lexData'''')     <- parseExprStatement lexData'''
        (change, lexData''''')  <- parsePostExp lexData''''
        lexData''''''           <- verifyAndConsume CloseParen lexData'''''
        (stmts, lexData''''''') <- parseStatement lexData''''''
        case test of
             (NullExprNode _) ->
                     pure (ForLoopNode ini (ConstantNode 1) change stmts dat, lexData''''''')
             _                ->
                     pure (ForLoopNode ini test change stmts dat, lexData''''''')


parsePostExp :: [LexDat] -> ParserState (Tree, [LexDat])
parsePostExp lexData = do
        (tree, lexData') <- parseForLoopPostExp lexData
        nextTokIsNot SemiColon lexData'
        pure (tree, lexData')


parseForLoopPostExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseForLoopPostExp (d@LexDat{tok=SemiColon}:_) =
        throwError $ SyntaxError (UnexpectedLexDat d)
parseForLoopPostExp lexData@(LexDat{tok=CloseParen}:_) = do
        dat <- makeNodeDat lexData
        pure (NullExprNode dat, lexData)
parseForLoopPostExp lexData = parseExpression lexData


parseDoWhile :: [LexDat] -> ParserState (Tree, [LexDat])
parseDoWhile lexData@(LexDat{tok=Keyword Do}:LexDat{tok=OpenBrace}:_) = do
        dat                <- makeNodeDat lexData
        lexData'           <- verifyAndConsume (Keyword Do) lexData
        (stmts, lexData'') <- parseStatement lexData'
        case lexData'' of
             (LexDat{tok=Keyword While}:LexDat{tok=OpenParen}:rest) -> do
                     (test, lexData''') <- parseExpression rest
                     lexData''''        <- verifyAndConsume CloseParen lexData'''
                     lexData'''''       <- verifyAndConsume SemiColon lexData''''
                     pure (DoWhileNode stmts test dat, lexData''''')
             (_:d@LexDat{tok=OpenParen}:_) ->
                     throwError $ SyntaxError (MissingKeyword While d)
             (d@LexDat{tok=Keyword While}:_:_) ->
                     throwError $ SyntaxError (MissingToken OpenParen d)
             _ -> throwError $ ParserError (LexDataError lexData')
parseDoWhile (d:_) = throwError $ SyntaxError (MissingToken OpenBrace d)
parseDoWhile [] = throwError $ ParserError (LexDataError [])


parseWhileStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseWhileStatement lexData = do
        dat                 <- makeNodeDat lexData
        lexData'            <- verifyAndConsume (Keyword While) lexData
        (test, lexData'')   <- parseConditionalParen lexData'
        (stmts, lexData''') <- parseStatement lexData''
        pure (WhileNode test stmts dat, lexData''')


parseIfStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseIfStatement lexData = do
        dat                     <- makeNodeDat lexData
        lexData'                <- verifyAndConsume (Keyword If) lexData
        (test, lexData'')       <- parseConditionalParen lexData'
        (stmts, lexData''')     <- parseStatement lexData''
        (possElse, lexData'''') <- parseOptionalElse lexData'''
        pure (IfNode test stmts possElse dat, lexData'''')


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
        dat               <- makeNodeDat lexData
        lexData'          <- verifyAndConsume (Keyword Return) lexData
        (tree, lexData'') <- parseExpression lexData'
        lexData'''        <- verifyAndConsume SemiColon lexData''
        pure (ReturnNode tree dat, lexData''')


parseNullStatement :: [LexDat] -> ParserState (Tree, [LexDat])
parseNullStatement lexData = do
        dat      <- makeNodeDat lexData
        lexData' <- verifyAndConsume SemiColon lexData
        pure (NullExprNode dat, lexData')
