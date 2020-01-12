
module CheckerSpec (checkerTest) where


import Data.Either
import Test.Hspec


import AST
import Checker
import Error
import Operator
import Type


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
                   ]
                  )
