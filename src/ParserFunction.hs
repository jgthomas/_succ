
module ParserFunction (parseFunction) where


import AST              (Tree (..))
import Error            (CompilerError (ParserError, SyntaxError),
                         ParserError (..), SyntaxError (..))
import LexDat           (LexDat (..))
import ParserExpression (parseExpression)
import ParserShared     (consumeTok, mkDat, parsePassIn, parseType,
                         verifyAndConsume)
import ParserStatement  (parseBlock)
import ParState         (ParserState, throwError)
import Tokens           (OpTok (..), Token (..))


parseFunction :: [LexDat] -> ParserState (Tree, [LexDat])
parseFunction [] = throwError $ ParserError (LexDataError [])
parseFunction lexData@(a:_) = do
        nodeDat         <- mkDat a
        typ             <- parseType lexData
        name            <- parseFuncName lexData
        (params, lexData') <- parseFuncParams lexData
        (items, lexData'') <- parseFuncBody [] lexData'
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


parseFuncBody :: [Tree] -> [LexDat] -> ParserState (Maybe [Tree], [LexDat])
parseFuncBody _ (LexDat{tok=SemiColon}:rest) = pure (Nothing, rest)
parseFuncBody stmts (LexDat{tok=OpenBrace}:rest) = do
        (tree, lexData') <- parseBlock stmts rest
        lexData''        <- verifyAndConsume CloseBrace lexData'
        pure (Just tree, lexData'')
parseFuncBody _ lexData = throwError $ ParserError (LexDataError lexData)
