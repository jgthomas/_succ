{-|
Module       : ParserStatement
Description  : Parses statements

Parses lexed tokens representing statements.
-}
module Parser.ParserStatement (parseStatement) where


import Control.Monad            (unless)

import Parser.ParserDeclaration (parseDeclaration)
import Parser.ParserExpression  (parseExpression)
import Parser.ParState          (ParserState, throwError)
import Parser.TokConsume        (checkAndConsume)
import Parser.TokToNodeData     (makeNodeDat)
import Types.AST                (Tree (..))
import Types.Error              (CompilerError (ParserError, SyntaxError),
                                 ParserError (..), SyntaxError (..))
import Types.Tokens


parseStatement :: [Token] -> ParserState (Tree, [Token])
parseStatement [] = throwError $ ParserError (LexDataError [])
parseStatement tokens@(first:_) =
        case first of
             Keyword Return _        -> parseReturnStmt tokens
             Keyword If _            -> parseIfStatement tokens
             Keyword While _         -> parseWhileStatement tokens
             Keyword Do _            -> parseDoWhile tokens
             Keyword For _           -> parseForLoop tokens
             Keyword Break _         -> parseBreak tokens
             Keyword Continue _      -> parseContinue tokens
             OpenBracket OpenBrace _ -> parseCompoundStmt tokens
             _                       -> parseExprStatement tokens


parseCompoundStmt :: [Token] -> ParserState (Tree, [Token])
parseCompoundStmt tokens = do
        dat               <- makeNodeDat tokens
        tokens'           <- checkAndConsume (Open OpenBrace) tokens
        (items, tokens'') <- parseStatementBlock [] tokens'
        tokens'''         <- checkAndConsume (Close CloseBrace) tokens''
        pure (CompoundStmtNode items dat, tokens''')


parseStatementBlock :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseStatementBlock stmts tokens@(CloseBracket CloseBrace _:_) =
        pure (reverse stmts, tokens)
parseStatementBlock stmts tokens = do
        (tree, tokens') <- parseBlockItem tokens
        parseStatementBlock (tree:stmts) tokens'


parseBlockItem :: [Token] -> ParserState (Tree, [Token])
parseBlockItem tokens@(Keyword Int _:Ident _ _:_) =
        parseDeclaration tokens
parseBlockItem tokens@(Keyword Int _:OpTok Asterisk _:_) =
        parseDeclaration tokens
parseBlockItem tokens = parseStatement tokens


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
parseExprStatement tokens@(Separator SemiColon _:_) = parseNullStatement tokens
parseExprStatement tokens = do
        dat             <- makeNodeDat tokens
        (tree, tokens') <- parseExpression tokens
        tokens''        <- checkAndConsume (Sep SemiColon) tokens'
        pure (ExprStmtNode tree dat, tokens'')


parseBreak :: [Token] -> ParserState (Tree, [Token])
parseBreak tokens@(Keyword Break _:Separator SemiColon _:rest) = do
        dat <- makeNodeDat tokens
        pure (BreakNode dat, rest)
parseBreak (Keyword Break _:d:_) =
        throwError $ SyntaxError (MissingToken (Separator SemiColon dummyLexDat) d)
parseBreak tokens = throwError $ ParserError (LexDataError tokens)


parseContinue :: [Token] -> ParserState (Tree, [Token])
parseContinue tokens@(Keyword Continue _:Separator SemiColon _:rest) = do
        dat <- makeNodeDat tokens
        pure (ContinueNode dat, rest)
parseContinue (Keyword Continue _:d:_) =
        throwError $ SyntaxError (MissingToken (Separator SemiColon dummyLexDat) d)
parseContinue tokens = throwError $ ParserError (LexDataError tokens)


parseForLoop :: [Token] -> ParserState (Tree, [Token])
parseForLoop tokens = do
        dat                    <- makeNodeDat tokens
        tokens'                <- checkAndConsume (Word For) tokens
        tokens''               <- checkAndConsume (Open OpenParen) tokens'
        (ini, tokens''')       <- parseBlockItem tokens''
        (test, tokens'''')     <- parseExprStatement tokens'''
        (change, tokens''''')  <- parsePostExp tokens''''
        tokens''''''           <- checkAndConsume (Close CloseParen) tokens'''''
        (stmts, tokens''''''') <- parseStatement tokens''''''
        let testCond = case test of
                            NullExprNode _ -> ConstantNode 1 dat
                            _              -> test
        pure (ForLoopNode ini testCond change stmts dat, tokens''''''')


parsePostExp :: [Token] -> ParserState (Tree, [Token])
parsePostExp tokens = do
        (tree, tokens') <- parseForLoopPostExp tokens
        nextTokIsNot (Separator SemiColon dummyLexDat) tokens'
        pure (tree, tokens')


parseForLoopPostExp :: [Token] -> ParserState (Tree, [Token])
parseForLoopPostExp (token@(Separator SemiColon _):_) =
        throwError $ SyntaxError (UnexpectedLexDat token)
parseForLoopPostExp tokens@(CloseBracket CloseParen _:_) = do
        dat <- makeNodeDat tokens
        pure (NullExprNode dat, tokens)
parseForLoopPostExp tokens = parseExpression tokens


parseDoWhile :: [Token] -> ParserState (Tree, [Token])
parseDoWhile tokens = do
        dat                <- makeNodeDat tokens
        tokens'            <- checkAndConsume (Word Do) tokens
        (stmts, tokens'')  <- parseStatement tokens'
        tokens'''          <- checkAndConsume (Word While) tokens''
        (test, tokens'''') <- parseConditionalParen tokens'''
        tokens'''''        <- checkAndConsume (Sep SemiColon) tokens''''
        pure (DoWhileNode stmts test dat, tokens''''')


parseWhileStatement :: [Token] -> ParserState (Tree, [Token])
parseWhileStatement tokens = do
        dat                <- makeNodeDat tokens
        tokens'            <- checkAndConsume (Word While) tokens
        (test, tokens'')   <- parseConditionalParen tokens'
        (stmts, tokens''') <- parseStatement tokens''
        pure (WhileNode test stmts dat, tokens''')


parseIfStatement :: [Token] -> ParserState (Tree, [Token])
parseIfStatement tokens = do
        dat                    <- makeNodeDat tokens
        tokens'                <- checkAndConsume (Word If) tokens
        (test, tokens'')       <- parseConditionalParen tokens'
        (stmts, tokens''')     <- parseStatement tokens''
        (possElse, tokens'''') <- parseOptionalElse tokens'''
        pure (IfNode test stmts possElse dat, tokens'''')


parseConditionalParen :: [Token] -> ParserState (Tree, [Token])
parseConditionalParen tokens = do
        tokens'             <- checkAndConsume (Open OpenParen) tokens
        (test, tokens'')    <- parseExpression tokens'
        tokens'''           <- checkAndConsume (Close CloseParen) tokens''
        pure (test, tokens''')


parseOptionalElse :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalElse (Keyword Else _:rest) = do
        (tree, tokens') <- parseStatement rest
        pure (Just tree, tokens')
parseOptionalElse tokens = pure (Nothing, tokens)


parseReturnStmt :: [Token] -> ParserState (Tree, [Token])
parseReturnStmt tokens = do
        dat              <- makeNodeDat tokens
        tokens'          <- checkAndConsume (Word Return) tokens
        (tree, tokens'') <- parseExpression tokens'
        tokens'''        <- checkAndConsume (Sep SemiColon) tokens''
        pure (ReturnNode tree dat, tokens''')


parseNullStatement :: [Token] -> ParserState (Tree, [Token])
parseNullStatement tokens = do
        dat     <- makeNodeDat tokens
        tokens' <- checkAndConsume (Sep SemiColon) tokens
        pure (NullExprNode dat, tokens')



nextTokIsNot :: Token -> [Token] -> ParserState ()
nextTokIsNot _ []    = throwError $ ParserError (LexDataError [])
nextTokIsNot t [a]   = isNotTok t a
nextTokIsNot t (a:_) = isNotTok t a


isNotTok :: Token -> Token -> ParserState ()
isNotTok t1 t2 = unless (t1 /= t2) $ throwError $ SyntaxError (UnexpectedLexDat t2)
