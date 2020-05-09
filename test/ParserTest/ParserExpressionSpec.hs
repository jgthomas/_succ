
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import Parser.ParserExpression
import Parser.ParState         as ParState
import TestUtility             (makeLexData, makeNodeDat)
import Types.AST
import Types.Error
import Types.LexDat
import Types.Tokens


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



lexDataSample :: [LexDat]
lexDataSample = makeLexData [Ident "a", SemiColon]


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Dummy test" $ do
                it "Should print dummy data" $
                  (getParsed . extractParsed $ lexDataSample)
                  `shouldBe`
                  ProgramNode [VarNode "a" makeNodeDat]

