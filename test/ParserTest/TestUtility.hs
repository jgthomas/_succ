
module ParserTest.TestUtility where


import Parser.ParserExpression
import Parser.ParState         as ParState
import TestUtility             (makeLexDat)
import Types.AST
import Types.Error
import Types.LexDat
import Types.Tokens


makeLexData :: [Token] -> [LexDat]
makeLexData toks = map makeLexDat toks


extractTree :: [Token] -> Tree
extractTree toks = do
        getParsed . extractParsed . makeLexData $ toks


getParsed :: Either CompilerError Tree -> Tree
getParsed (Right tree) = tree
getParsed (Left err)   = error $ show err


extractParsed :: [LexDat] -> Either CompilerError Tree
extractParsed lexData = runParState runTheParse lexData startState


runTheParse :: [LexDat] -> ParserState Tree
runTheParse lexData = do
        (item, _) <- parseExpression lexData
        putState $ ProgramNode [item]
        ProgramNode .reverse <$> getState
