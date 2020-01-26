
module ParserSpec (parserTest) where


import Data.Either
import Test.Hspec

import AST
import Error
import LexDat
import Operator
import Parser
import Tokens
import Type


mkLexDat :: Token -> LexDat
mkLexDat tok = LexDat tok 0


parserTest :: IO ()
parserTest = hspec $ do
        describe "Parse tokens to AST" $ do
                it "Should parse valid variable declaration" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "a",
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [DeclarationNode
                    "a"
                    IntVar
                    Nothing
                   ]
                  )

                it "Should parse valid variable assignment" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "a",
                     OpTok EqualSign,
                     ConstInt 10,
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [DeclarationNode
                    "a"
                    IntVar
                    (Just
                     (AssignmentNode
                      "a"
                      (ConstantNode 10)
                      Assignment
                     )
                    )
                   ]
                  )

                it "Should parse valid pointer declaration" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     OpTok Asterisk,
                     Ident "a",
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [PointerNode
                    "a"
                    IntPointer
                    Nothing
                   ]
                  )

                it "Should parse valid pointer assignment" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     OpTok Asterisk,
                     Ident "a",
                     OpTok EqualSign,
                     OpTok Ampersand,
                     Ident "b",
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [PointerNode
                    "a"
                    IntPointer
                    (Just
                     (AssignmentNode
                      "a"
                      (AddressOfNode "b")
                      Assignment
                     )
                    )
                   ]
                  )

                it "Should parse valid function declaration without parameters" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "cat",
                     OpenParen,
                     CloseParen,
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "cat"
                    []
                    Nothing
                   ]
                  )

                it "Should parse valid function declaration with parameters" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "cat",
                     OpenParen,
                     Keyword Int,
                     Ident "a",
                     CloseParen,
                     SemiColon]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "cat"
                    [ParamNode
                     IntVar
                     (VarNode "a")
                    ]
                    Nothing
                   ]
                  )

                it "Should parse valid function definition" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "main",
                     OpenParen,
                     CloseParen,
                     OpenBrace,
                     Keyword Return,
                     ConstInt 2,
                     SemiColon,
                     CloseBrace]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "main"
                    []
                    (Just
                     [ReturnNode
                      (ConstantNode 2)
                     ]
                    )
                   ]
                  )

                it "Should parse valid function definition with parameter" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "main",
                     OpenParen,
                     Keyword Int,
                     Ident "a",
                     CloseParen,
                     OpenBrace,
                     Keyword Return,
                     Ident "a",
                     SemiColon,
                     CloseBrace]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "main"
                    [ParamNode
                     IntVar
                     (VarNode "a")
                    ]
                    (Just
                     [ReturnNode
                      (VarNode "a")
                     ]
                    )
                   ]
                  )

                it "Should parse valid function returning a unary node" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "main",
                     OpenParen,
                     CloseParen,
                     OpenBrace,
                     Keyword Return,
                     OpTok MinusSign,
                     ConstInt 2,
                     SemiColon,
                     CloseBrace]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "main"
                    []
                    (Just
                     [ReturnNode
                      (UnaryNode
                       (ConstantNode 2)
                       (Unary Negate)
                      )
                     ]
                    )
                   ]
                  )

                it "Should parse valid function returning a binary node" $
                  fromRight
                  (ProgramNode [])
                  (parse
                   (map mkLexDat
                    [Keyword Int,
                     Ident "main",
                     OpenParen,
                     CloseParen,
                     OpenBrace,
                     Keyword Return,
                     ConstInt 2,
                     OpTok PlusSign,
                     ConstInt 2,
                     SemiColon,
                     CloseBrace]
                   )
                  )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "main"
                    []
                    (Just
                     [ReturnNode
                      (BinaryNode
                       (ConstantNode 2)
                       (ConstantNode 2)
                       Plus
                      )
                     ]
                    )
                   ]
                  )

        describe "Throw correct errors" $ do
                it "Should throw error on invalid variable identifier" $
                  fromLeft
                  ImpossibleError
                  (parse
                   (map mkLexDat [Keyword Int, OpenBrace, SemiColon])
                  )
                  `shouldBe`
                  (SyntaxError (NonValidIdentifier (LexDat{tok=OpenBrace,line=0})))

                it "Should throw error on empty input" $
                  fromLeft
                  ImpossibleError
                  (parse [])
                  `shouldBe`
                  ParserError (LexDataError [])

                it "Should throw error on invalid type" $
                  fromLeft
                  ImpossibleError
                  (parse
                   (map mkLexDat [Keyword Break, Ident "a", SemiColon])
                  )
                  `shouldBe`
                  (TypeError (BadType  (LexDat{tok=Keyword Break,line=0})))

                it "Should throw error if semicolon not final token" $
                  fromLeft
                  ImpossibleError
                  (parse
                   (map mkLexDat [Keyword Int, Ident "a", Comma])
                  )
                  `shouldBe`
                  SyntaxError (MissingToken SemiColon)
