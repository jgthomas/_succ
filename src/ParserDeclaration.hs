
module ParserDeclaration (parseDeclaration) where


import           AST              (Tree (..))
import           Error            (CompilerError (ParserError, SyntaxError),
                                   ParserError (..), SyntaxError (..))
import           LexDat           (LexDat (..))
import           ParserExpression (parseExpression)
import           ParserShared     (consumeTok, makeNodeDat, parseType,
                                   verifyAndConsume)
import           ParState         (ParserState, throwError)
import           Tokens           (CloseBracket (..), OpenBracket (..),
                                   Token (..))
import qualified Tokens           (isAssign)


data Declaration = ValueDec
                 | PointerDec
                 | ArrayDec
                 deriving (Eq)


parseDeclaration :: [LexDat] -> ParserState (Tree, [LexDat])
parseDeclaration lexData@(
        _:LexDat{tok=Ident name}:
        LexDat{tok=OpenBracket OpenSqBracket}:
        LexDat{tok=CloseBracket CloseSqBracket}:_)      = parseDec ArrayDec name lexData
parseDeclaration lexData@(
        _:LexDat{tok=Ident name}:
        LexDat{tok=OpenBracket OpenSqBracket}:
        LexDat{tok=ConstInt _}:
        LexDat{tok=CloseBracket CloseSqBracket}:_)      = parseDec ArrayDec name lexData
parseDeclaration lexData@(_:LexDat{tok=Ident name}:_)   = parseDec ValueDec name lexData
parseDeclaration lexData@(_:_:LexDat{tok=Ident name}:_) = parseDec PointerDec name lexData
parseDeclaration (_:c:_:_) = throwError $ SyntaxError (NonValidIdentifier c)
parseDeclaration lexData   = throwError $ ParserError (LexDataError lexData)


parseDec :: Declaration -> String -> [LexDat] -> ParserState (Tree, [LexDat])
parseDec decType name lexData = do
        dat               <- makeNodeDat lexData
        typ               <- parseType lexData
        lexData'          <- findVarName lexData
        varDat            <- makeNodeDat lexData'
        (tree, lexData'') <- parseOptAssign lexData'
        let var = VarNode name varDat
        case decType of
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


findVarName :: [LexDat] -> ParserState [LexDat]
findVarName [] = pure []
findVarName lexData@(LexDat{tok=Ident _}:_) = pure lexData
findVarName lexData = do
        lexData' <- consumeTok lexData
        findVarName lexData'
