
module ParserTest.TokClassSpec where


import Test.Hspec

import Parser.TokClass
import Types.Tokens


tokClassTest :: IO ()
tokClassTest = hspec $ do
        describe "Assign tokens to correct class" $ do
                it "Should correctly identify assignments" $
                  map isAssign [EqualSign,
                                PlusEqual,
                                MinusEqual,
                                AsteriskEqual,
                                BackslashEqual,
                                PercentEqual,
                                AmpEqual,
                                CaretEqual,
                                PipeEqual,
                                DoubleLArrowEqual,
                                DoubleRArrowEqual]
                  `shouldBe`
                  [True, True, True, True, True, True, True, True, True, True, True]

                it "Should correctly identify post position unary operators" $
                  map isPostPos [PlusPlus, MinusMinus] `shouldBe` [True, True]
