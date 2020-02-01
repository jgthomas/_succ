
module CheckerSpec (checkerTest) where


import           Data.Either
import           Test.Hspec

import qualified TestUtility (mkNodeDat)

import           AST
import           Checker
import           Error
import           Type


checkerTest :: IO ()
checkerTest = hspec $ do
        describe "Check valid AST" $ do
                it "Should return a valid AST unchanged" $
                  fromRight
                  (ProgramNode [])
                  (check
                    (ProgramNode
                     [FunctionNode
                      IntVar
                      "main"
                      []
                      (Just [
                       ReturnNode
                        (ConstantNode 2)
                      ])
                      TestUtility.mkNodeDat
                     ]
                    )
                   )
                  `shouldBe`
                  (ProgramNode
                   [FunctionNode
                    IntVar
                    "main"
                    []
                    (Just [
                     ReturnNode
                      (ConstantNode 2)
                    ])
                    TestUtility.mkNodeDat
                   ]
                  )

        describe "Check invalid AST" $ do
                it "Should error if return type doesn't match function declaration" $
                  fromLeft
                  ImpossibleError
                  (check
                   (ProgramNode
                    [PointerNode
                     "a"
                     IntPointer
                     Nothing,
                     FunctionNode
                     IntVar
                     "dog"
                     []
                     (Just
                      [ReturnNode
                       (VarNode "a")
                      ]
                     )
                     TestUtility.mkNodeDat
                    ]
                   )
                  )
                  `shouldBe`
                  TypeError (TypeMismatch [IntVar] [IntPointer])

                it "Should error if returning undeclared variable" $
                  fromLeft
                  ImpossibleError
                  (check
                   (ProgramNode
                    [FunctionNode
                     IntVar
                     "dog"
                     []
                     (Just
                      [ReturnNode
                       (VarNode "a")
                      ]
                     )
                     TestUtility.mkNodeDat
                    ]
                   )
                  )
                  `shouldBe`
                  ScopeError (UnrecognisedNode (VarNode "a"))
