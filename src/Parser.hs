{-|
Module       : Parser
Description  : Produces an abstract syntax tree

Converts a list of tokens into an abstract syntax tree
representing the C program.
-}
module Parser (parse) where


import           AST               (Tree (..))
import           Error             (CompilerError (ImpossibleError, ParserError, SyntaxError),
                                    ParserError (..), SyntaxError (..))
import           LexDat            (LexDat (..))
import           ParState          (ParserState, runParState, throwError)
import qualified ParState          (getState, putState, startState)
import           Tokens            (OpTok (..), Token (..))


import           ParserDeclaration (parsePointerDec, parseValueDec)
import           ParserExpression  (parseExpression)
import           ParserShared
import           ParserStatement   (parseBlock)


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
