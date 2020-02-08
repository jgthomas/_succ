
module ParserDeclaration (parseDeclaration) where


import           AST              (Tree (..))
import           Error            (CompilerError (ParserError, SyntaxError),
                                   ParserError (..), SyntaxError (..))
import           LexDat           (LexDat (..))
import           ParserExpression (parseExpression)
import           ParserShared     (consumeNToks, consumeTok, makeNodeDat,
                                   parseType, verifyAndConsume)
import           ParState         (ParserState, throwError)
import           Tokens           (Token (..))
import qualified Tokens           (isAssign)


parseDeclaration :: [LexDat] -> ParserState (Tree, [LexDat])
parseDeclaration lexData@(_:LexDat{tok=Ident name}:_)   = parseValueDec name lexData
parseDeclaration lexData@(_:_:LexDat{tok=Ident name}:_) = parsePointerDec name lexData
parseDeclaration (_:c:_:_) = throwError $ SyntaxError (NonValidIdentifier c)
parseDeclaration lexData   = throwError $ ParserError (LexDataError lexData)


parseValueDec :: String -> [LexDat] -> ParserState (Tree, [LexDat])
parseValueDec name lexData = do
        dat               <- makeNodeDat lexData
        typ               <- parseType lexData
        lexData'          <- consumeTok lexData
        varDat            <- makeNodeDat lexData'
        (tree, lexData'') <- parseOptAssign lexData'
        pure (DeclarationNode (VarNode name varDat) typ tree dat, lexData'')


parsePointerDec :: String -> [LexDat] -> ParserState (Tree, [LexDat])
parsePointerDec name lexData = do
        dat               <- makeNodeDat lexData
        typ               <- parseType lexData
        lexData'          <- consumeNToks 2 lexData
        varDat            <- makeNodeDat lexData'
        (tree, lexData'') <- parseOptAssign lexData'
        pure (PointerNode (VarNode name varDat) typ tree dat, lexData'')


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
