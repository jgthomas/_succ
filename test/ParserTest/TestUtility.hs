
module ParserTest.TestUtility
        (extractExpressionTree,
         extractExpressionError,
         extractDeclarationTree,
         extractDeclarationError,
         extractFunctionTree,
         extractFunctionError,
         extractStatementTree,
         extractStatementError,
         extractFullProgramTree,
         extractFullProgramError
        ) where


import Parser.Parser            (parse)
import Parser.ParserDeclaration (parseDeclaration)
import Parser.ParserExpression  (parseExpression)
import Parser.ParserFunction    (parseFunction)
import Parser.ParserStatement   (parseStatement)
import Parser.ParState          (ParserState, evaluate, getState, putState,
                                 startState)
import Types.AST
import Types.Error
import Types.Tokens


-- | Extracts the abstract syntax tree for an expression
extractExpressionTree :: [Token] -> Tree
extractExpressionTree toks = extractTree parseExpression $ addExtraToken toks

-- | Extracts the error message from parsing an expression
extractExpressionError :: [Token] -> CompilerError
extractExpressionError toks = extractError parseExpression toks


-- | Extracts the abstract syntax tree for a declaration
extractDeclarationTree :: [Token] -> Tree
extractDeclarationTree toks = extractTree parseDeclaration $ addExtraToken toks

extractDeclarationError :: [Token] -> CompilerError
extractDeclarationError toks = extractError parseDeclaration toks


extractStatementTree :: [Token] -> Tree
extractStatementTree toks = extractTree parseStatement $ addExtraToken toks

extractStatementError :: [Token] -> CompilerError
extractStatementError toks = extractError parseStatement toks


-- | Extracts the abstract syntax tree for a function
extractFunctionTree :: [Token] -> Tree
extractFunctionTree toks = extractTree parseFunction $ addExtraToken toks

-- | Extracts the error message from parsing a function
extractFunctionError :: [Token] -> CompilerError
extractFunctionError toks = extractError parseFunction toks


-- | Extracts the abstract syntax tree for a full program
extractFullProgramTree :: [Token] -> Tree
extractFullProgramTree toks = getTree . parse $ toks


extractFullProgramError :: [Token] -> CompilerError
extractFullProgramError toks = getError . parse $ toks


{-
- Some parsers throw an error if the list of tokens becomes empty, so add
- an extra token here to prevent that error.
-}
addExtraToken :: [Token] -> [Token]
addExtraToken toks = toks ++ [Separator SemiColon dummyLexDat]


extractTree :: ([Token] -> ParserState (Tree, [Token]))
            -> [Token]
            -> Tree
extractTree f toks = do
        getTree . extract $ toks
        where
                extract = extractParsed f


getTree :: Either CompilerError Tree -> Tree
getTree (Right tree) = tree
getTree (Left err)   = error $ show err


extractError :: ([Token] -> ParserState (Tree, [Token]))
            -> [Token]
            -> CompilerError
extractError f toks = do
        getError . extract $ toks
        where
                extract = extractParsed f


getError :: Either CompilerError Tree -> CompilerError
getError (Right tree) = error $ show tree
getError (Left err)   = err


extractParsed :: ([Token] -> ParserState (Tree, [Token]))
              -> [Token]
              -> Either CompilerError Tree
extractParsed f lexData = evaluate run lexData startState
        where
                run = runTheParse f


runTheParse :: ([Token] -> ParserState (Tree, [Token]))
            -> [Token]
            -> ParserState Tree
runTheParse f tokens = do
        (item, _) <- f tokens
        putState [item]
        ProgramNode . reverse <$> getState
