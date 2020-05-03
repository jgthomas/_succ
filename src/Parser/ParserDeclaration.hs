
module Parser.ParserDeclaration (parseDeclaration) where


import           Lexer.LexTab            (LexDat (..))
import           Parser.ParserExpression (parseExpression)
import           Parser.ParserShared     (consumeTok, makeNodeDat, parseType,
                                          verifyAndConsume)
import           Parser.ParState         (ParserState, throwError)
import           Types.AST               (ArrayNode (..), Tree (..))
import           Types.Error             (CompilerError (ParserError, SyntaxError),
                                          ParserError (..), SyntaxError (..))
import           Types.Tokens            (CloseBracket (..), OpenBracket (..),
                                          Token (..))
import qualified Types.Tokens            as Tokens (isAssign)


data Declaration = ValueDec
                 | PointerDec
                 | ArrayDec
                 | ArrayDecExplicit Int
                 deriving (Eq)


data ArrayLen = Undeclared
              | Declared Int
              deriving (Eq)


parseDeclaration :: [LexDat] -> ParserState (Tree, [LexDat])
parseDeclaration lexData@(
        _:LexDat{tok=Ident name}:
        LexDat{tok=OpenBracket OpenSqBracket}:
        LexDat{tok=CloseBracket CloseSqBracket}:_)      = parseDec ArrayDec name lexData
parseDeclaration lexData@(
        _:LexDat{tok=Ident name}:
        LexDat{tok=OpenBracket OpenSqBracket}:
        LexDat{tok=ConstInt n}:
        LexDat{tok=CloseBracket CloseSqBracket}:_)      = parseDec (ArrayDecExplicit n) name lexData
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
             ArrayDec   -> do
                     len <- findArraylen var (inferredLen tree) Undeclared
                     pure (ArrayNode (ArrayDeclareNode len var typ tree dat), lexData'')
             (ArrayDecExplicit n) -> do
                     len <- findArraylen var (inferredLen tree) (Declared n)
                     pure (ArrayNode (ArrayDeclareNode len var typ tree dat), lexData'')


findArraylen :: Tree -> ArrayLen -> ArrayLen -> ParserState Int
findArraylen node (Declared n) (Declared m)
        | n == m = pure n
        | otherwise = throwError $ SyntaxError (LengthMismatch node n m)
findArraylen _ (Declared n) Undeclared = pure n
findArraylen _ Undeclared (Declared m) = pure m
findArraylen node Undeclared Undeclared = throwError $ SyntaxError (UndeclaredLen node)


inferredLen ::  Maybe Tree -> ArrayLen
inferredLen (Just (ArrayNode (ArrayItemsNode _ items _))) = Declared (length items)
inferredLen Nothing = Undeclared
inferredLen _       = Undeclared


parseOptAssign :: [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseOptAssign lexData = do
        (tree, lexData') <- parseOptionalAssign lexData
        lexData''        <- verifyAndConsume SemiColon lexData'
        pure (tree, lexData'')


parseOptionalAssign :: [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseOptionalAssign lexData@(_:d@LexDat{tok=OpTok _}:_) = parseAssign d lexData
parseOptionalAssign (ident@LexDat{tok=Ident _}:
                     LexDat{tok=OpenBracket OpenSqBracket}:
                     LexDat{tok=ConstInt _}:
                     LexDat{tok=CloseBracket CloseSqBracket}:
                     d@LexDat{tok=OpTok _}:rest) = parseAssign d (ident:rest)
parseOptionalAssign (ident@LexDat{tok=Ident _}:
                     LexDat{tok=OpenBracket OpenSqBracket}:
                     LexDat{tok=CloseBracket CloseSqBracket}:
                     d@LexDat{tok=OpTok _}:rest) = parseAssign d (ident:rest)
parseOptionalAssign (_:LexDat{tok=OpenBracket OpenSqBracket}:
                     LexDat{tok=ConstInt _}:
                     LexDat{tok=CloseBracket CloseSqBracket}:
                     rest) = pure (Nothing, rest)
parseOptionalAssign (_:LexDat{tok=OpenBracket OpenSqBracket}:
                     LexDat{tok=CloseBracket CloseSqBracket}:
                     rest) = pure (Nothing, rest)
parseOptionalAssign lexData = do
        lexData' <- consumeTok lexData
        pure (Nothing, lexData')


parseAssign :: LexDat -> [LexDat] -> ParserState (Maybe Tree, [LexDat])
parseAssign opDat@LexDat{tok=OpTok op} lexData
        | Tokens.isAssign op = do
                (tree, lexData') <- parseExpression lexData
                pure (Just tree, lexData')
        | otherwise = throwError $ SyntaxError (UnexpectedLexDat opDat)
parseAssign _ lexData = throwError $ ParserError (LexDataError lexData)


findVarName :: [LexDat] -> ParserState [LexDat]
findVarName [] = pure []
findVarName lexData@(LexDat{tok=Ident _}:_) = pure lexData
findVarName lexData = do
        lexData' <- consumeTok lexData
        findVarName lexData'
