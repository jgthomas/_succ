{-|
Module       : ParserFunction
Description  : Parses functions

Parses lexed tokens representing functions.
-}
module Parser.ParserFunction (parseFunction) where


import Parser.ParserExpression (parseExpression)
import Parser.ParserSequence   (parseBracketedSeq)
import Parser.ParserStatement  (parseStatement)
import Parser.ParserType       (parseType)
import Parser.ParState         (ParserState, throwError)
import Parser.TokConsume       (checkAndConsume, consumeNToks, consumeTok)
import Parser.TokToNodeData    (makeNodeDat)
import Types.AST               (Tree (..))
import Types.Error             (CompilerError (ParserError, SyntaxError),
                                ParserError (..), SyntaxError (..))
import Types.Tokens


-- | Parse tokens for a function into an AST
parseFunction :: [Token] -> ParserState (Tree, [Token])
parseFunction tokens = do
        nodeDat           <- makeNodeDat tokens
        typ               <- parseType tokens
        name              <- parseFuncName tokens
        (params, tokens') <- parseFuncParams tokens
        (items, tokens'') <- parseFuncBody tokens'
        pure (FunctionNode typ name params items nodeDat, tokens'')


parseFuncName :: [Token] -> ParserState String
parseFuncName (_:Ident name _:_)   = pure name
parseFuncName (_:_:Ident name _:_) = pure name
parseFuncName (token:_) = throwError $ SyntaxError (NonValidIdentifier token)
parseFuncName [] = throwError $ ParserError (LexDataError [])


parseFuncParams :: [Token] -> ParserState ([Tree], [Token])
parseFuncParams tokens@(_:OpTok Asterisk _:_:OpenBracket OpenParen _:_) = do
        tokens' <- consumeNToks 3 tokens
        parseAllParams tokens'
parseFuncParams tokens@(_:Ident _ _:OpenBracket OpenParen _:_) = do
        tokens' <- consumeNToks 2 tokens
        parseAllParams tokens'
parseFuncParams tokens = throwError $ ParserError (LexDataError tokens)


parseAllParams :: [Token] -> ParserState ([Tree], [Token])
parseAllParams tokens = do
        (params, tokens') <- parseParams [] tokens
        tokens''          <- checkAndConsume (Close CloseParen) tokens'
        pure (params, tokens'')


parseParams :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseParams params tokens = parseBracketedSeq params tokens parseTheParams


parseTheParams :: [Tree] -> [Token] -> ParserState ([Tree], [Token])
parseTheParams params tokens@(Keyword _ _:_) = do
        (tree, tokens') <- parseParam tokens
        parseParams (tree:params) tokens'
parseTheParams _ tokens = throwError $ ParserError (LexDataError tokens)


parseParam :: [Token] -> ParserState (Tree, [Token])
parseParam tokens = do
        nodeDat          <- makeNodeDat tokens
        typ              <- parseType tokens
        tokens'          <- consumeTok tokens
        (tree, tokens'') <- parseParamValue tokens'
        case tree of
             VarNode{} -> pure (ParamNode typ tree nodeDat, tokens'')
             _         -> throwError $ ParserError (TreeError tree)


parseParamValue :: [Token] -> ParserState (Tree, [Token])
parseParamValue (OpTok Asterisk _:rest) = parseExpression rest
parseParamValue tokens@(Ident _ _:_)    = parseExpression tokens
parseParamValue tokens = throwError $ ParserError (LexDataError tokens)


parseFuncBody :: [Token] -> ParserState (Maybe Tree, [Token])
parseFuncBody (SemiColon _:rest) = pure (Nothing, rest)
parseFuncBody body@(OpenBracket OpenBrace _:_) = do
        (tree, tokens') <- parseStatement body
        pure (Just tree, tokens')
parseFuncBody tokens = throwError $ ParserError (LexDataError tokens)
