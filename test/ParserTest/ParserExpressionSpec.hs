
module ParserTest.ParserExpressionSpec (parserExpressionTest) where


import Test.Hspec


parserExpressionTest :: IO ()
parserExpressionTest = hspec $ do
        describe "Dummy test" $ do
                it "Should print dummy data" $
                 True `shouldBe` True

