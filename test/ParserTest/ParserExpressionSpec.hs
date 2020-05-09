
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import Parser.ParserExpression
import Parser.ParState         as ParState
import ParserTest.TestUtility
import TestUtility             (makeNodeDat)
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


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Dummy test" $ do
                it "Should print dummy data" $
                  (getParsed . extractParsed . makeLexData $ [Ident "a", SemiColon])
                  `shouldBe`
                  ProgramNode [VarNode "a" makeNodeDat]

