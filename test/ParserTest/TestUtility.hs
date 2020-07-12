
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
import Parser.ParState
import TestUtility              (makeLexDat)
import Types.AST
import Types.Error
import Types.LexDat
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
extractFullProgramTree toks = getTree . parse . makeLexData $ toks


extractFullProgramError :: [Token] -> CompilerError
extractFullProgramError toks = getError . parse . makeLexData $ toks


{-
- Some parsers throw an error if the list of tokens becomes empty, so add
- an extra token here to prevent that error.
-}
addExtraToken :: [Token] -> [Token]
addExtraToken toks = toks ++ [SemiColon]


makeLexData :: [Token] -> [LexDat]
makeLexData toks = map makeLexDat toks


extractTree :: ([LexDat] -> ParserState (Tree, [LexDat]))
            -> [Token]
            -> Tree
extractTree f toks = do
        getTree . extract . makeLexData $ toks
        where
                extract = extractParsed f


getTree :: Either CompilerError Tree -> Tree
getTree (Right tree) = tree
getTree (Left err)   = error $ show err


extractError :: ([LexDat] -> ParserState (Tree, [LexDat]))
            -> [Token]
            -> CompilerError
extractError f toks = do
        getError . extract . makeLexData $ toks
        where
                extract = extractParsed f


getError :: Either CompilerError Tree -> CompilerError
getError (Right tree) = error $ show tree
getError (Left err)   = err


extractParsed :: ([LexDat] -> ParserState (Tree, [LexDat]))
              -> [LexDat]
              -> Either CompilerError Tree
extractParsed f lexData = runParState run lexData startState
        where
                run = runTheParse f


runTheParse :: ([LexDat] -> ParserState (Tree, [LexDat]))
            -> [LexDat]
            -> ParserState Tree
runTheParse f lexData = do
        (item, _) <- f lexData
        putState $ ProgramNode [item]
        ProgramNode . reverse <$> getState
