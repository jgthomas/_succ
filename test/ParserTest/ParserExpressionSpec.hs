
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec

import Parser.ParserExpression
import Parser.ParState         as ParState
import TestUtility             (makeLexData, makeNodeDat)
import Types.AST
import Types.Error
import Types.LexDat
import Types.Tokens


getParsed :: Either CompilerError Tree -> Maybe Tree
getParsed (Right tree) = Just tree
getParsed (Left err)   = error $ show err


extractParsed :: [LexDat] -> Either CompilerError Tree
extractParsed lexData = runParState runTheParse lexData startState


runTheParse :: [LexDat] -> ParserState Tree
runTheParse [] = ProgramNode . reverse <$> getState
runTheParse lexData = do
        items            <- getState
        (item, lexData') <- parseExpression lexData
        putState $ ProgramNode (item:items)
        runTheParse lexData'



lexDataSample :: [LexDat]
lexDataSample = makeLexData [Ident "a"]


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Dummy test" $ do
                it "Should print dummy data" $
                  (getParsed . extractParsed $ lexDataSample)
                  `shouldBe`
                  Just (ProgramNode [VarNode "a" makeNodeDat])

