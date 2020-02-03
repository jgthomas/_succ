
module ParserExpression (parseExpression) where


import           AST          (Tree (..))
import           Error        (CompilerError (ImpossibleError, ParserError, SyntaxError),
                               ParserError (..), SyntaxError (..))
import           LexDat       (LexDat (..))
import qualified Operator     (tokToAssignOp, tokToBinOp, tokToPostUnaryOp,
                               tokToUnaryOp)
import           ParserShared (consumeTok, makeNodeDat, parsePassIn,
                               verifyAndConsume)
import           ParState     (ParserState, throwError)
import           Tokens       (OpTok (..), OpTokType (..), Token (..))
import qualified Tokens       (isAssign, isPostPos, kind)


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


parseAssignment :: Tree -> [LexDat] -> ParserState (Tree, [LexDat])
parseAssignment tree (LexDat{tok=OpTok op}:rest) = do
                   (asgn, lexData') <- parseExpression rest
                   let asgnOp = Operator.tokToAssignOp op
                   dat <- makeNodeDat lexData'
                   case tree of
                     (VarNode a) ->
                             pure (AssignmentNode a asgn asgnOp dat, lexData')
                     (DereferenceNode a) ->
                             pure (AssignDereferenceNode a asgn asgnOp dat, lexData')
                     _ -> throwError $ ParserError (TreeError tree)
parseAssignment _ lexData = throwError $ ParserError (LexDataError lexData)


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
             LexDat{tok=SemiColon}        -> parseNullExpression lexData
             LexDat{tok=ConstInt _}       -> parseConstant lexData
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


parseNullExpression :: [LexDat] -> ParserState (Tree, [LexDat])
parseNullExpression lexData = do
        dat      <- makeNodeDat lexData
        lexData' <- verifyAndConsume SemiColon lexData
        pure (NullExprNode dat, lexData')


parseConstant :: [LexDat] -> ParserState (Tree, [LexDat])
parseConstant lexData@(LexDat{tok=ConstInt n}:rest) = do
        dat <- makeNodeDat lexData
        pure (ConstantNode n dat, rest)
parseConstant lexData = throwError $ ParserError (LexDataError lexData)


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
