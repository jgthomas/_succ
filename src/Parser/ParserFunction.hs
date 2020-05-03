
module Parser.ParserFunction (parseFunction) where


import AST                     (Tree (..))
import Error.Error             (CompilerError (ParserError, SyntaxError),
                                ParserError (..), SyntaxError (..))
import LexTab                  (LexDat (..))
import Parser.ParserExpression (parseExpression)
import Parser.ParserShared     (consumeNToks, consumeTok, makeNodeDat,
                                parseBracketedSeq, parseType, verifyAndConsume)
import Parser.ParserStatement  (parseStatementBlock)
import Parser.ParState         (ParserState, throwError)
import Tokens                  (CloseBracket (..), OpTok (..), OpenBracket (..),
                                Token (..))


parseFunction :: [LexDat] -> ParserState (Tree, [LexDat])
parseFunction lexData = do
        nodeDat            <- makeNodeDat lexData
        typ                <- parseType lexData
        name               <- parseFuncName lexData
        (params, lexData') <- parseFuncParams lexData
        (items, lexData'') <- parseFuncBody lexData'
        pure (FunctionNode typ name params items nodeDat, lexData'')


parseFuncName :: [LexDat] -> ParserState String
parseFuncName (_:LexDat{tok=Ident name}:_)   = pure name
parseFuncName (_:_:LexDat{tok=Ident name}:_) = pure name
parseFuncName (d:_) = throwError $ SyntaxError (NonValidIdentifier d)
parseFuncName [] = throwError $ ParserError (LexDataError [])


parseFuncParams :: [LexDat] -> ParserState ([Tree], [LexDat])
parseFuncParams lexData@(_:LexDat{tok=OpTok Asterisk}:_:LexDat{tok=OpenBracket OpenParen}:_) = do
        lexData' <- consumeNToks 3 lexData
        parseAllParams lexData'
parseFuncParams lexData@(_:LexDat{tok=Ident _}:LexDat{tok=OpenBracket OpenParen}:_) = do
        lexData' <- consumeNToks 2 lexData
        parseAllParams lexData'
parseFuncParams lexData = throwError $ ParserError (LexDataError lexData)


parseAllParams :: [LexDat] -> ParserState ([Tree], [LexDat])
parseAllParams lexData = do
        (params, lexData') <- parseParams [] lexData
        lexData''          <- verifyAndConsume (CloseBracket CloseParen) lexData'
        pure (params, lexData'')


parseParams :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseParams prms lexData = parseBracketedSeq prms lexData parseTheParams


parseTheParams :: [Tree] -> [LexDat] -> ParserState ([Tree], [LexDat])
parseTheParams prms lexData@(LexDat{tok=Keyword _}:_) = do
        (tree, lexData') <- parseParam lexData
        parseParams (tree:prms) lexData'
parseTheParams _ lexData = throwError $ ParserError (LexDataError lexData)


parseParam :: [LexDat] -> ParserState (Tree, [LexDat])
parseParam lexData = do
        nodeDat           <- makeNodeDat lexData
        typ               <- parseType lexData
        lexData'          <- consumeTok lexData
        (tree, lexData'') <- parseParamValue lexData'
        case tree of
             VarNode{} -> pure (ParamNode typ tree nodeDat, lexData'')
             _         -> throwError $ ParserError (TreeError tree)


parseParamValue :: [LexDat] -> ParserState (Tree, [LexDat])
parseParamValue (LexDat{tok=OpTok Asterisk}:rest) = parseExpression rest
parseParamValue lexData@(LexDat{tok=Ident _}:_)   = parseExpression lexData
parseParamValue lexData = throwError $ ParserError (LexDataError lexData)


parseFuncBody :: [LexDat] -> ParserState (Maybe [Tree], [LexDat])
parseFuncBody (LexDat{tok=SemiColon}:rest) = pure (Nothing, rest)
parseFuncBody (LexDat{tok=OpenBracket OpenBrace}:rest) = do
        (tree, lexData') <- parseStatementBlock [] rest
        lexData''        <- verifyAndConsume (CloseBracket CloseBrace) lexData'
        pure (Just tree, lexData'')
parseFuncBody lexData = throwError $ ParserError (LexDataError lexData)
