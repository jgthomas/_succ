{-|
Module       : Parser
Description  : Produces an abstract syntax tree

Converts a list of tokens into an abstract syntax tree
representing the C program.
-}
module Parser (parse) where


import           Control.Monad (unless)

import           AST           (Tree (..))
import           Error         (CompilerError (..), ParserError (..),
                                SyntaxError (..), TypeError (..))
import           LexDat        (LexDat (..))
import qualified Operator      (tokToAssignOp, tokToBinOp, tokToPostUnaryOp,
                                tokToUnaryOp)
import           ParState      (ParserState, runParState, throwError)
import qualified ParState      (getState, putState, startState)
import           Tokens        (Keyword (..), OpTok (..), OpTokType (..),
                                Token (..))
import qualified Tokens        (isAssign, isPostPos, kind)
import           Type          (Type (..))


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
parseFunction lexData = do
        typ             <- parseType lexData
        name            <- parseFuncName lexData
        (params, lexData') <- parseFuncParams lexData
        (items, lexData'') <- parseFuncBlockItems [] lexData'
        pure (FunctionNode typ name params items, lexData'')


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


parseAssignment :: Tree -> [LexDat] -> ParserState (Tree, [LexDat])
parseAssignment tree (LexDat{tok=OpTok op}:rest) = do
                   (asgn, lexData') <- parseExpression rest
                   let asgnOp = Operator.tokToAssignOp op
                   case tree of
                     (VarNode a) ->
                             pure (AssignmentNode a asgn asgnOp, lexData')
                     (DereferenceNode a) ->
                             pure (AssignDereferenceNode a asgn asgnOp, lexData')
                     _ -> throwError $ ParserError (TreeError tree)
parseAssignment _ lexData = throwError $ ParserError (LexDataError lexData)


parseExpression :: [LexDat] -> ParserState (Tree, [LexDat])
parseExpression lexData = do
        (tree, lexData') <- parseTernaryExp lexData
        case lexData' of
             (d@LexDat{tok=OpTok op}:rest)
                | Tokens.isAssign op  -> parseAssignment tree lexData'
                | Tokens.isPostPos op -> do
                        let unOp = Operator.tokToPostUnaryOp op
                        pure (UnaryNode tree unOp, rest)
                | otherwise ->
                        throwError $ SyntaxError (UnexpectedLexDat d)
             _ -> pure (tree, lexData')


parseTernaryExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseTernaryExp lexData = do
        (cond, lexData') <- parseLogicalOrExp lexData
        case lexData' of
             (LexDat{tok=QuestMark}:rest) -> do
                     (expr1, lexData'')   <- parseExpression rest
                     lexData'''           <- verifyAndConsume Colon lexData''
                     (expr2, lexData'''') <- parseTernaryExp lexData'''
                     pure (TernaryNode cond expr1 expr2, lexData'''')
             _ -> pure (cond, lexData')


parseLogicalOrExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseLogicalOrExp lexData = do
        (orTree, lexData') <- parseLogicalAndExp lexData
        parseBinaryExp orTree lexData' parseLogicalAndExp (Tokens.kind LogicalOR)


parseLogicalAndExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseLogicalAndExp lexData = do
        (andTree, lexData') <- parseBitwiseOR lexData
        parseBinaryExp andTree lexData' parseBitwiseOR (Tokens.kind LogicalAND)


parseBitwiseOR :: [LexDat] -> ParserState (Tree, [LexDat])
parseBitwiseOR lexData = do
        (orTree, lexData') <- parseBitwiseXOR lexData
        parseBinaryExp orTree lexData' parseBitwiseXOR (Tokens.kind BitwiseOR)


parseBitwiseXOR :: [LexDat] -> ParserState (Tree, [LexDat])
parseBitwiseXOR lexData = do
        (xorTree, lexData') <- parseBitwiseAND lexData
        parseBinaryExp xorTree lexData' parseBitwiseAND (Tokens.kind BitwiseXOR)


parseBitwiseAND :: [LexDat] -> ParserState (Tree, [LexDat])
parseBitwiseAND lexData = do
        (andTree, lexData') <- parseEqualityExp lexData
        parseBinaryExp andTree lexData' parseEqualityExp (Tokens.kind BitwiseAND)


parseEqualityExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseEqualityExp lexData = do
        (equTree, lexData') <- parseRelationalExp lexData
        parseBinaryExp equTree lexData' parseRelationalExp (Tokens.kind Equality)


parseRelationalExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseRelationalExp lexData = do
        (relaTree, lexData') <- parseBitShiftExp lexData
        parseBinaryExp relaTree lexData' parseBitShiftExp (Tokens.kind Relational)


parseBitShiftExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseBitShiftExp lexData = do
        (shiftTree, lexData') <- parseAdditiveExp lexData
        parseBinaryExp shiftTree lexData' parseAdditiveExp (Tokens.kind Shift)


parseAdditiveExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseAdditiveExp lexData = do
        (termTree, lexData') <- parseTerm lexData
        parseBinaryExp termTree lexData' parseTerm (Tokens.kind Term)


parseTerm :: [LexDat] -> ParserState (Tree, [LexDat])
parseTerm lexData = do
        (facTree, lexData') <- parseFactor lexData
        parseBinaryExp facTree lexData' parseFactor (Tokens.kind Factor)


parseFactor :: [LexDat] -> ParserState (Tree, [LexDat])
parseFactor [] = throwError $ ParserError (LexDataError [])
parseFactor lexData@(next:rest) =
        case next of
             LexDat{tok=SemiColon}        -> pure (NullExprNode, rest)
             LexDat{tok=ConstInt n}       -> pure (ConstantNode n, rest)
             LexDat{tok=Ident _}          -> parseIdent lexData
             LexDat{tok=OpTok Ampersand}  -> parseAddressOf rest
             LexDat{tok=OpTok Asterisk}   -> parseDereference rest
             LexDat{tok=OpTok MinusSign}  -> parseUnary lexData
             LexDat{tok=OpTok Tilde}      -> parseUnary lexData
             LexDat{tok=OpTok Bang}       -> parseUnary lexData
             LexDat{tok=OpTok PlusPlus}   -> parseUnary lexData
             LexDat{tok=OpTok MinusMinus} -> parseUnary lexData
             LexDat{tok=OpTok PlusSign}   -> parseUnary lexData
             LexDat{tok=OpenParen}        -> parseParenExp rest
             _ -> throwError $ ParserError (LexDataError lexData)


parseUnary :: [LexDat] -> ParserState (Tree, [LexDat])
parseUnary (LexDat{tok=OpTok op}:rest) = do
        (tree, lexData') <- parseFactor rest
        let unOp = Operator.tokToUnaryOp op
        pure (UnaryNode tree unOp, lexData')
parseUnary lexData = throwError $ ParserError (LexDataError lexData)


parseIdent :: [LexDat] -> ParserState (Tree, [LexDat])
parseIdent lexData@(LexDat{tok=Ident _}:LexDat{tok=OpenParen}:_) =
        parseFuncCall lexData
parseIdent (LexDat{tok=Ident a}:rest) = pure (VarNode a, rest)
parseIdent (a:_) = throwError $ SyntaxError (UnexpectedLexDat a)
parseIdent lexData  = throwError $ ParserError (LexDataError lexData)


parseParenExp :: [LexDat] -> ParserState (Tree, [LexDat])
parseParenExp lexData = do
        (tree, lexData') <- parseExpression lexData
        lexData''        <- verifyAndConsume CloseParen lexData'
        pure (tree, lexData'')


parseAddressOf :: [LexDat] -> ParserState (Tree, [LexDat])
parseAddressOf (LexDat{tok=Ident n}:rest) = pure (AddressOfNode n, rest)
parseAddressOf (a:_)   = throwError $ SyntaxError (NonValidIdentifier a)
parseAddressOf lexData = throwError $ ParserError (LexDataError lexData)


parseDereference :: [LexDat] -> ParserState (Tree, [LexDat])
parseDereference (LexDat{tok=Ident n}:rest) = pure (DereferenceNode n, rest)
parseDereference (a:_)   = throwError $ SyntaxError (NonValidIdentifier a)
parseDereference lexData = throwError $ ParserError (LexDataError lexData)


parseFuncCall :: [LexDat] -> ParserState (Tree, [LexDat])
parseFuncCall lexData@(LexDat{tok=Ident a}:LexDat{tok=OpenParen}:_) = do
        lexData'          <- consumeTok lexData
        (tree, lexData'') <- parseArgs [] lexData'
        pure (FuncCallNode a tree, lexData'')
parseFuncCall (d@LexDat{tok=Ident _}:_:_) =
        throwError $ SyntaxError (MissingToken OpenParen d)
parseFuncCall (a:LexDat{tok=OpenParen}:_) =
        throwError $ SyntaxError (NonValidIdentifier a)
parseFuncCall (a:_:_) =
        throwError $ SyntaxError (UnexpectedLexDat a)
parseFuncCall lexData =
        throwError $ ParserError (LexDataError lexData)


parseArgs :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseArgs args lexData = parsePassIn args lexData parseTheArgs


parseTheArgs :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseTheArgs as lexData = do
        (tree, lexData') <- parseExpression lexData
        parseArgs (tree:as) lexData'


parsePassIn :: [Tree]
            -> [LexDat]
            -> ([Tree] -> [LexDat] -> ParserState ([Tree], [LexDat]))
            -> ParserState ([Tree], [LexDat])
parsePassIn _ [] _ = throwError $ ParserError (LexDataError [])
parsePassIn xs (LexDat{tok=OpenParen}:LexDat{tok=CloseParen}:rest) _ =
        pure (xs, rest)
parsePassIn xs (LexDat{tok=CloseParen}:rest) _ = pure (reverse xs, rest)
parsePassIn _ (d@LexDat{tok=Comma}:LexDat{tok=CloseParen}:_) _ =
        throwError $ SyntaxError (UnexpectedLexDat d)
parsePassIn xs (LexDat{tok=OpenParen}:rest) f = f xs rest
parsePassIn xs (LexDat{tok=Comma}:rest) f     = f xs rest
parsePassIn _ (a:_) _ = throwError $ SyntaxError (UnexpectedLexDat a)


parseBinaryExp :: Tree
               -> [LexDat]
               -> ([LexDat] -> ParserState (Tree, [LexDat]))
               -> [OpTok]
               -> ParserState (Tree, [LexDat])
parseBinaryExp _ [] _ _ = throwError $ ParserError (LexDataError [])
parseBinaryExp _ _ _ [] = throwError ImpossibleError
parseBinaryExp tree lexData@(LexDat{tok=OpTok op}:rest) f ops
        | op `elem` ops = do
                (ntree, lexData'') <- f rest
                let binOp = Operator.tokToBinOp op
                parseBinaryExp (BinaryNode tree ntree binOp) lexData'' f ops
        | otherwise = pure (tree, lexData)
parseBinaryExp tree lexData _ _ = pure (tree, lexData)


verifyAndConsume :: Token -> [LexDat] -> ParserState [LexDat]
verifyAndConsume t lexData = do
        nextTokIs t lexData
        consumeTok lexData


nextTokIs :: Token -> [LexDat] -> ParserState ()
nextTokIs _ []    = throwError $ ParserError (LexDataError [])
nextTokIs t [a]   = isTok t a
nextTokIs t (a:_) = isTok t a


nextTokIsNot :: Token -> [LexDat] -> ParserState ()
nextTokIsNot _ []    = throwError $ ParserError (LexDataError [])
nextTokIsNot t [a]   = isNotTok t a
nextTokIsNot t (a:_) = isNotTok t a


isTok :: Token -> LexDat -> ParserState ()
isTok t a = unless (t == tok a) $ throwError $ SyntaxError (MissingToken t a)


isNotTok :: Token -> LexDat -> ParserState ()
isNotTok t a = unless ( t /= tok a) $ throwError $ SyntaxError (UnexpectedLexDat a)


consumeTok :: [LexDat] -> ParserState [LexDat]
consumeTok []          = throwError $ ParserError (LexDataError [])
consumeTok [_]         = pure []
consumeTok (_:lexData) = pure lexData


consumeNToks :: Int -> [LexDat] -> ParserState [LexDat]
consumeNToks 0 lexData = pure lexData
consumeNToks n lexData = do
        lexData' <- consumeTok lexData
        consumeNToks (pred n) lexData'


parseType :: [LexDat] -> ParserState Type
parseType (LexDat{tok=Keyword Int}:LexDat{tok=OpTok Asterisk}:_) =
        pure IntPointer
parseType (LexDat{tok=Keyword Int}:_) = pure IntVar
parseType (a:_) = throwError $ TypeError (BadType a)
parseType lexData  = throwError $ ParserError (LexDataError lexData)


nullExpr :: [LexDat] -> ParserState (Tree, [LexDat])
nullExpr lexData = pure (NullExprNode, lexData)
