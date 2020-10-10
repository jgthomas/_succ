{-|
Module       : ParserDeclaration
Description  : Parses declarations

Converts a list of tokens into an abstract syntax tree
representing a declaration
-}
module Parser.ParserDeclaration (parseDeclaration) where


import           Parser.ParserExpression (parseExpression)
import           Parser.ParserType       (parseType)
import           Parser.ParState         (ParserState, throwError)
import qualified Parser.TokClass         as TokClass (isAssign)
import           Parser.TokConsume       (checkAndConsume, consumeTok)
import           Parser.TokToNodeData    (makeNodeDat)
import           Types.AST               (ArrayNode (..), Tree (..))
import           Types.Error             (CompilerError (ParserError, SyntaxError),
                                          ParserError (..), SyntaxError (..))
import           Types.Tokens


data Declaration = ValueDec
                 | PointerDec
                 | ArrayDec
                 | ArrayDecExplicit Int


data ArrayLen = Undeclared
              | Declared Int


-- | Parse tokens for a declaration into an AST
parseDeclaration :: [Token] -> ParserState (Tree, [Token])
parseDeclaration tokens@(_:Ident name _:
                         OpenBracket OpenSqBracket _:
                         CloseBracket CloseSqBracket _:_) = parseDec ArrayDec name tokens
parseDeclaration tokens@(_:Ident name _:
                         OpenBracket OpenSqBracket _:
                         ConstInt n _:
                         CloseBracket CloseSqBracket _:_) = parseDec (ArrayDecExplicit n) name tokens
parseDeclaration tokens@(_:Ident name _:_)   = parseDec ValueDec name tokens
parseDeclaration tokens@(_:_:Ident name _:_) = parseDec PointerDec name tokens
parseDeclaration (_:c:_:_) = throwError $ SyntaxError (NonValidIdentifier c)
parseDeclaration tokens   = throwError $ ParserError (LexDataError tokens)


parseDec :: Declaration -> String -> [Token] -> ParserState (Tree, [Token])
parseDec decType name tokens = do
        dat              <- makeNodeDat tokens
        typ              <- parseType tokens
        tokens'          <- findVarName tokens
        varDat           <- makeNodeDat tokens'
        (tree, tokens'') <- parseOptAssign tokens'
        let var = VarNode name varDat
        case decType of
             PointerDec -> pure (PointerNode var typ tree dat, tokens'')
             ValueDec   -> pure (DeclarationNode var typ tree dat, tokens'')
             ArrayDec   -> do
                     len <- findArraylen var (inferredLen tree) Undeclared
                     pure (ArrayNode (ArrayDeclareNode len var typ tree dat), tokens'')
             (ArrayDecExplicit n) -> do
                     len <- findArraylen var (inferredLen tree) (Declared n)
                     pure (ArrayNode (ArrayDeclareNode len var typ tree dat), tokens'')


findArraylen :: Tree -> ArrayLen -> ArrayLen -> ParserState Int
findArraylen node (Declared n) (Declared m)
        | n == m = pure n
        | otherwise = throwError $ SyntaxError (LengthMismatch node n m)
findArraylen _ (Declared n) Undeclared = pure n
findArraylen _ Undeclared (Declared m) = pure m
findArraylen node Undeclared Undeclared = throwError $ SyntaxError (UndeclaredLen node)


inferredLen ::  Maybe Tree -> ArrayLen
inferredLen (Just (ArrayNode (ArrayItemsNode _ items _))) = Declared (length items)
inferredLen Nothing                                       = Undeclared
inferredLen _                                             = Undeclared


parseOptAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptAssign tokens = do
        (tree, tokens') <- parseOptionalAssign tokens
        tokens''        <- checkAndConsume (Sep SemiColon) tokens'
        pure (tree, tokens'')


parseOptionalAssign :: [Token] -> ParserState (Maybe Tree, [Token])
parseOptionalAssign tokens@(_:opTok@(OpTok _ _):_) = parseAssign opTok tokens
parseOptionalAssign (ident@(Ident _ _):
                     OpenBracket OpenSqBracket _:
                     ConstInt _ _:
                     CloseBracket CloseSqBracket _:
                     opTok@(OpTok _ _):rest) = parseAssign opTok (ident:rest)
parseOptionalAssign (ident@(Ident _ _):
                     OpenBracket OpenSqBracket _:
                     CloseBracket CloseSqBracket _:
                     opTok@(OpTok _ _):rest) = parseAssign opTok (ident:rest)
parseOptionalAssign (_:OpenBracket OpenSqBracket _:
                     ConstInt _ _:
                     CloseBracket CloseSqBracket _:
                     rest) = pure (Nothing, rest)
parseOptionalAssign tokens = do
        tokens' <- consumeTok tokens
        pure (Nothing, tokens')


parseAssign :: Token -> [Token] -> ParserState (Maybe Tree, [Token])
parseAssign opTok@(OpTok op _) tokens
        | TokClass.isAssign op = do
                (tree, tokens') <- parseExpression tokens
                pure (Just tree, tokens')
        | otherwise = throwError $ SyntaxError (UnexpectedLexDat opTok)
parseAssign _ tokens = throwError $ ParserError (LexDataError tokens)


findVarName :: [Token] -> ParserState [Token]
findVarName [] = pure []
findVarName tokens@(Ident _ _:_) = pure tokens
findVarName tokens = do
        tokens' <- consumeTok tokens
        findVarName tokens'
