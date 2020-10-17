-- |
-- Module       : ParserExpression
-- Description  : Parses expression
--
-- Parses lexed tokens representing expressions.
module Parser.ParserExpression
  ( parseExpression,
  )
where

import Parser.ParState (ParserState, throwError)
import Parser.ParserSequence (parseBracketedSeq)
import Parser.TokClass (OpTokType (..))
import qualified Parser.TokClass as TokClass
import Parser.TokConsume (checkAndConsume, consume)
import qualified Parser.TokConvert as TokConvert
import Parser.TokToNodeData (makeNodeDat)
import Types.AST (ArrayNode (..), Tree (..))
import Types.Error
  ( CompilerError (ImpossibleError, ParserError, SyntaxError),
    ParserError (..),
    SyntaxError (..),
  )
import Types.Tokens

-- | Parse tokens for an expression into an AST
parseExpression :: [Token] -> ParserState (Tree, [Token])
parseExpression tokens = do
  (tree, tokens') <- parseTernaryExp tokens
  case tokens' of
    (opTok@(OpTok op _) : rest)
      | TokClass.isAssign op -> parseAssignment tree tokens'
      | TokClass.isPostPos op -> do
        dat <- makeNodeDat tokens'
        let unOp = TokConvert.tokToPostUnaryOp op
        pure (UnaryNode tree unOp dat, rest)
      | otherwise ->
        throwError $ SyntaxError (UnexpectedLexDat opTok)
    _ -> pure (tree, tokens')

parseAssignment :: Tree -> [Token] -> ParserState (Tree, [Token])
parseAssignment tree (OpTok op _ : rest) = do
  (asgn, tokens') <- parseExpression rest
  let asgnOp = TokConvert.tokToAssignOp op
  dat <- makeNodeDat tokens'
  case tree of
    arrPosNode@(ArrayNode ArrayItemAssign {}) ->
      pure (ArrayNode (ArrayAssignPosNode arrPosNode asgn asgnOp dat), tokens')
    varNode@VarNode {} ->
      pure (AssignmentNode varNode asgn asgnOp dat, tokens')
    derefNode@DereferenceNode {} ->
      pure (AssignDereferenceNode derefNode asgn asgnOp dat, tokens')
    _ -> throwError $ ParserError (TreeError tree)
parseAssignment _ tokens = throwError $ ParserError (LexDataError tokens)

parseTernaryExp :: [Token] -> ParserState (Tree, [Token])
parseTernaryExp tokens = do
  dat <- makeNodeDat tokens
  (cond, tokens') <- parseLogicalOrExp tokens
  case tokens' of
    (Separator QuestMark _ : rest) -> do
      (expr1, tokens'') <- parseExpression rest
      tokens''' <- checkAndConsume (Sep Colon) tokens''
      (expr2, tokens'''') <- parseTernaryExp tokens'''
      pure (TernaryNode cond expr1 expr2 dat, tokens'''')
    _ -> pure (cond, tokens')

parseLogicalOrExp :: [Token] -> ParserState (Tree, [Token])
parseLogicalOrExp tokens = do
  (orTree, tokens') <- parseLogicalAndExp tokens
  parseBinaryExp orTree tokens' parseLogicalAndExp (TokClass.kind LogicalOR)

parseLogicalAndExp :: [Token] -> ParserState (Tree, [Token])
parseLogicalAndExp tokens = do
  (andTree, tokens') <- parseBitwiseOR tokens
  parseBinaryExp andTree tokens' parseBitwiseOR (TokClass.kind LogicalAND)

parseBitwiseOR :: [Token] -> ParserState (Tree, [Token])
parseBitwiseOR tokens = do
  (orTree, tokens') <- parseBitwiseXOR tokens
  parseBinaryExp orTree tokens' parseBitwiseXOR (TokClass.kind BitwiseOR)

parseBitwiseXOR :: [Token] -> ParserState (Tree, [Token])
parseBitwiseXOR tokens = do
  (xorTree, tokens') <- parseBitwiseAND tokens
  parseBinaryExp xorTree tokens' parseBitwiseAND (TokClass.kind BitwiseXOR)

parseBitwiseAND :: [Token] -> ParserState (Tree, [Token])
parseBitwiseAND tokens = do
  (andTree, tokens') <- parseEqualityExp tokens
  parseBinaryExp andTree tokens' parseEqualityExp (TokClass.kind BitwiseAND)

parseEqualityExp :: [Token] -> ParserState (Tree, [Token])
parseEqualityExp tokens = do
  (equTree, tokens') <- parseRelationalExp tokens
  parseBinaryExp equTree tokens' parseRelationalExp (TokClass.kind Equality)

parseRelationalExp :: [Token] -> ParserState (Tree, [Token])
parseRelationalExp tokens = do
  (relaTree, tokens') <- parseBitShiftExp tokens
  parseBinaryExp relaTree tokens' parseBitShiftExp (TokClass.kind Relational)

parseBitShiftExp :: [Token] -> ParserState (Tree, [Token])
parseBitShiftExp tokens = do
  (shiftTree, tokens') <- parseAdditiveExp tokens
  parseBinaryExp shiftTree tokens' parseAdditiveExp (TokClass.kind Shift)

parseAdditiveExp :: [Token] -> ParserState (Tree, [Token])
parseAdditiveExp tokens = do
  (termTree, tokens') <- parseTerm tokens
  parseBinaryExp termTree tokens' parseTerm (TokClass.kind Term)

parseTerm :: [Token] -> ParserState (Tree, [Token])
parseTerm tokens = do
  (facTree, tokens') <- parseFactor tokens
  parseBinaryExp facTree tokens' parseFactor (TokClass.kind Factor)

parseFactor :: [Token] -> ParserState (Tree, [Token])
parseFactor [] = throwError $ ParserError (LexDataError [])
parseFactor tokens@(next : rest) =
  case next of
    Separator SemiColon _ -> parseNullExpression tokens
    ConstInt _ _ -> parseConstant tokens
    OpTok Ampersand _ -> parseAddressOf tokens
    OpTok Asterisk _ -> parseDereference tokens
    OpTok MinusSign _ -> parseUnary tokens
    OpTok Tilde _ -> parseUnary tokens
    OpTok Bang _ -> parseUnary tokens
    OpTok PlusPlus _ -> parseUnary tokens
    OpTok MinusMinus _ -> parseUnary tokens
    OpTok PlusSign _ -> parseUnary tokens
    OpenBracket OpenParen _ -> parseParenExp rest
    Ident _ _ -> parseIdent tokens
    _ -> throwError $ ParserError (LexDataError tokens)

parseIdent :: [Token] -> ParserState (Tree, [Token])
parseIdent tokens@(Ident _ _ : OpenBracket OpenParen _ : _) =
  parseFuncCall tokens
parseIdent tokens@(Ident _ _ : OpenBracket OpenBrace _ : _) =
  parseArrayItems tokens
parseIdent tokens@(Ident _ _ : OpenBracket OpenSqBracket _ : _) =
  parseArrayIndex tokens
parseIdent tokens@(Ident a _ : rest) = do
  dat <- makeNodeDat tokens
  pure (VarNode a dat, rest)
parseIdent (token : _) = throwError $ SyntaxError (UnexpectedLexDat token)
parseIdent tokens = throwError $ ParserError (LexDataError tokens)

parseArrayItems :: [Token] -> ParserState (Tree, [Token])
parseArrayItems tokens@(Ident name _ : OpenBracket OpenBrace _ : _) = do
  varDat <- makeNodeDat tokens
  tokens' <- consume 1 tokens
  dat <- makeNodeDat tokens'
  (items, tokens'') <- parseItems [] tokens'
  tokens''' <- checkAndConsume (Close CloseBrace) tokens''
  pure (ArrayNode (ArrayItemsNode (VarNode name varDat) items dat), tokens''')
parseArrayItems tokens = throwError $ ParserError (LexDataError tokens)

parseItems :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseItems items tokens = parseBracketedSeq items tokens parseTheItems

parseTheItems :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseTheItems items tokens = do
  (item, tokens') <- parseItem tokens
  parseItems (item : items) tokens'

parseItem :: [Token] -> ParserState (Tree, [Token])
parseItem tokens = do
  dat <- makeNodeDat tokens
  (item, tokens') <- parseExpression tokens
  pure (ArrayNode (ArraySingleItemNode item dat), tokens')

parseNullExpression :: [Token] -> ParserState (Tree, [Token])
parseNullExpression tokens = do
  dat <- makeNodeDat tokens
  tokens' <- checkAndConsume (Sep SemiColon) tokens
  pure (NullExprNode dat, tokens')

parseConstant :: [Token] -> ParserState (Tree, [Token])
parseConstant tokens@(ConstInt n _ : rest) = do
  dat <- makeNodeDat tokens
  pure (ConstantNode n dat, rest)
parseConstant tokens = throwError $ ParserError (LexDataError tokens)

parseUnary :: [Token] -> ParserState (Tree, [Token])
parseUnary tokens@(OpTok op _ : rest) = do
  dat <- makeNodeDat tokens
  (tree, tokens') <- parseFactor rest
  let unOp = TokConvert.tokToUnaryOp op
  pure (UnaryNode tree unOp dat, tokens')
parseUnary tokens = throwError $ ParserError (LexDataError tokens)

parseArrayIndex :: [Token] -> ParserState (Tree, [Token])
parseArrayIndex
  tokens@( Ident a _ :
             OpenBracket OpenSqBracket _ :
             ConstInt n _ :
             CloseBracket CloseSqBracket _ :
             opTok@(OpTok _ _) :
             rest
           ) = do
    dat <- makeNodeDat tokens
    pure (ArrayNode $ ArrayItemAssign n (VarNode a dat) dat, opTok : rest)
parseArrayIndex
  tokens@( Ident a _ :
             OpenBracket OpenSqBracket _ :
             ConstInt n _ :
             CloseBracket CloseSqBracket _ :
             rest
           ) = do
    dat <- makeNodeDat tokens
    pure (ArrayNode $ ArrayItemAccess n (VarNode a dat) dat, rest)
parseArrayIndex tokens = throwError $ ParserError (LexDataError tokens)

parseParenExp :: [Token] -> ParserState (Tree, [Token])
parseParenExp tokens = do
  (tree, tokens') <- parseExpression tokens
  tokens'' <- checkAndConsume (Close CloseParen) tokens'
  pure (tree, tokens'')

parseAddressOf :: [Token] -> ParserState (Tree, [Token])
parseAddressOf tokens@(OpTok Ampersand _ : Ident n _ : rest) = do
  dat <- makeNodeDat tokens
  pure (AddressOfNode n dat, rest)
parseAddressOf (_ : token : _) = throwError $ SyntaxError (NonValidIdentifier token)
parseAddressOf tokens = throwError $ ParserError (LexDataError tokens)

parseDereference :: [Token] -> ParserState (Tree, [Token])
parseDereference tokens@(OpTok Asterisk _ : Ident n _ : rest) = do
  dat <- makeNodeDat tokens
  pure (DereferenceNode n dat, rest)
parseDereference (_ : token : _) = throwError $ SyntaxError (NonValidIdentifier token)
parseDereference tokens = throwError $ ParserError (LexDataError tokens)

parseFuncCall :: [Token] -> ParserState (Tree, [Token])
parseFuncCall tokens@(Ident a _ : OpenBracket OpenParen _ : _) = do
  dat <- makeNodeDat tokens
  tokens' <- consume 1 tokens
  (tree, tokens'') <- parseArgs [] tokens'
  tokens''' <- checkAndConsume (Close CloseParen) tokens''
  pure (FuncCallNode a tree dat, tokens''')
parseFuncCall (token@(Ident _ _) : _ : _) =
  throwError $ SyntaxError (MissingToken (OpenBracket OpenParen dummyLexDat) token)
parseFuncCall (token : OpenBracket OpenParen _ : _) =
  throwError $ SyntaxError (NonValidIdentifier token)
parseFuncCall (token : _ : _) =
  throwError $ SyntaxError (UnexpectedLexDat token)
parseFuncCall tokens =
  throwError $ ParserError (LexDataError tokens)

parseArgs :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseArgs args tokens = parseBracketedSeq args tokens parseTheArgs

parseTheArgs :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseTheArgs as tokens = do
  (tree, tokens') <- parseArg tokens
  parseArgs (tree : as) tokens'

parseArg :: [Token] -> ParserState (Tree, [Token])
parseArg tokens = do
  dat <- makeNodeDat tokens
  (tree, tokens') <- parseExpression tokens
  pure (ArgNode tree dat, tokens')

parseBinaryExp ::
  Tree ->
  [Token] ->
  ([Token] -> ParserState (Tree, [Token])) ->
  [OpTok] ->
  ParserState (Tree, [Token])
parseBinaryExp _ [] _ _ = throwError $ ParserError (LexDataError [])
parseBinaryExp _ _ _ [] = throwError ImpossibleError
parseBinaryExp tree tokens@(OpTok op _ : rest) f ops
  | op `elem` ops = do
    dat <- makeNodeDat tokens
    (ntree, tokens'') <- f rest
    let binOp = TokConvert.tokToBinOp op
    parseBinaryExp (BinaryNode tree ntree binOp dat) tokens'' f ops
  | otherwise = pure (tree, tokens)
parseBinaryExp tree tokens _ _ = pure (tree, tokens)
