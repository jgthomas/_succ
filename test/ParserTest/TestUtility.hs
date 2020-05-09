
module ParserTest.TestUtility (extractExpressionTree) where


import Parser.ParserExpression
import Parser.ParState         as ParState
import TestUtility             (makeLexDat)
import Types.AST
import Types.Error
import Types.LexDat
import Types.Tokens


-- | Extracts the abstract syntax tree for an expression
extractExpressionTree :: [Token] -> Tree
extractExpressionTree toks = extractTree parseExpression toks


makeLexData :: [Token] -> [LexDat]
makeLexData toks = map makeLexDat toks


extractTree :: ([LexDat] -> ParserState (Tree, [LexDat]))
            -> [Token]
            -> Tree
extractTree f toks = do
        getParsed . extract . makeLexData $ toks
        where
                extract = extractParsed f


getParsed :: Either CompilerError Tree -> Tree
getParsed (Right tree) = tree
getParsed (Left err)   = error $ show err


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
