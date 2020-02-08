
module ParserDeclaration (parseDeclaration) where


import           AST              (Tree (..))
import           Error            (CompilerError (ParserError, SyntaxError),
                                   ParserError (..), SyntaxError (..))
import           LexDat           (LexDat (..))
import           ParserExpression (parseExpression)
import           ParserShared     (consumeNToks, consumeTok, makeNodeDat,
                                   parseType, verifyAndConsume)
import           ParState         (ParserState, throwError)
import           Tokens           (CloseBracket (..), OpenBracket (..),
                                   Token (..))
import qualified Tokens           (isAssign)


data Declaration = ValueDec
                 | PointerDec
                 | ArrayDec
                 deriving (Eq)


parseDeclaration :: [LexDat] -> ParserState (Tree, [LexDat])
parseDeclaration lexData@(_:LexDat{tok=Ident name}:_)   = parseDec name lexData 1
parseDeclaration lexData@(_:_:LexDat{tok=Ident name}:_) = parseDec name lexData 2
parseDeclaration (_:c:_:_) = throwError $ SyntaxError (NonValidIdentifier c)
parseDeclaration lexData   = throwError $ ParserError (LexDataError lexData)


parseDec :: String -> [LexDat] -> Int -> ParserState (Tree, [LexDat])
parseDec name lexData n = do
        dat               <- makeNodeDat lexData
        typ               <- parseType lexData
        lexData'          <- consumeNToks n lexData
        varDat            <- makeNodeDat lexData'
        (tree, lexData'') <- parseOptAssign lexData'
        let var = VarNode name varDat
        case declarationType lexData of
             PointerDec -> pure (PointerNode var typ tree dat, lexData'')
             ValueDec   -> pure (DeclarationNode var typ tree dat, lexData'')
             ArrayDec   -> undefined


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


declarationType :: [LexDat] -> Declaration
declarationType (_:_:LexDat{tok=Ident _}:_)                 = PointerDec
declarationType (_:LexDat{tok=Ident _}:
                 LexDat{tok=OpenBracket OpenSqBracket}:
                 LexDat{tok=CloseBracket CloseSqBracket}:_) = ArrayDec
declarationType (_:LexDat{tok=Ident _}:_)                   = ValueDec
declarationType _ = undefined

