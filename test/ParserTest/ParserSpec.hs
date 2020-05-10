
module ParserTest.ParserSpec (fullParserTest) where


import Test.Hspec

import ParserTest.TestUtility (extractFullProgramTree)
import TestUtility            (makeNodeDat)
import Types.AST
import Types.Tokens
import Types.Type


fullParserTest :: IO ()
fullParserTest = hspec $ do
        describe "Build abstract syntax trees for full programs" $ do

                it "Should build a tree for a simple program returning a constant" $
                  (extractFullProgramTree [Keyword Int,
                                           Ident "main",
                                           OpenBracket OpenParen,
                                           CloseBracket CloseParen,
                                           OpenBracket OpenBrace,
                                           Keyword Return,
                                           ConstInt 2,
                                           SemiColon,
                                           CloseBracket CloseBrace])
                  `shouldBe`
                  ProgramNode [FunctionNode
                               IntVar
                               "main"
                               []
                               (Just [ReturnNode (ConstantNode 2 makeNodeDat) makeNodeDat])
                               makeNodeDat]
