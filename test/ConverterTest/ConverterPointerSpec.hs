module ConverterTest.ConverterPointerSpec
  ( spec,
  )
where

import ConverterTest.TestUtility (extractSchema)
import Test.Hspec
import TestUtility (mockNodeDat)
import Types.AST
import Types.AssemblySchema
import Types.Operator
import Types.Type
import Types.Variables

spec :: Spec
spec = do
  describe "Build assembly schemas for pointers" $ do
    it "Should create a schema for a global pointer declaration" $
      ( extractSchema $
          ProgramNode
            [ PointerNode
                (VarNode "a" mockNodeDat)
                IntPointer
                Nothing
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntPointer,
            SkipSchema
          ]
    it "Should create a schema for a global pointer declaration with assignment" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              PointerNode
                (VarNode "b" mockNodeDat)
                IntPointer
                ( Just $
                    AssignmentNode
                      (VarNode "b" mockNodeDat)
                      (AddressOfNode "a" mockNodeDat)
                      Assignment
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema,
            DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0) UntrackedValue)
              ( StatementSchema $
                  AssignmentSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0) UntrackedValue)
                    (ExpressionSchema $ AddressOfSchema $ ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                    Global
              )
              Global
              IntPointer
          ]
    it "Should create a schema for a pointer dereference" $
      ( extractSchema $
          ProgramNode
            [ DeclarationNode
                (VarNode "a" mockNodeDat)
                IntVar
                Nothing
                mockNodeDat,
              PointerNode
                (VarNode "b" mockNodeDat)
                IntPointer
                ( Just $
                    AssignmentNode
                      (VarNode "b" mockNodeDat)
                      (AddressOfNode "a" mockNodeDat)
                      Assignment
                      mockNodeDat
                )
                mockNodeDat,
              DeclarationNode
                (VarNode "c" mockNodeDat)
                IntVar
                ( Just $
                    AssignmentNode
                      (VarNode "c" mockNodeDat)
                      (DereferenceNode "b" mockNodeDat)
                      Assignment
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
              SkipSchema
              Global
              IntVar,
            SkipSchema,
            DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0) UntrackedValue)
              ( StatementSchema $
                  AssignmentSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0) UntrackedValue)
                    (ExpressionSchema $ AddressOfSchema $ ExpressionSchema $ VariableSchema (GlobalVar "_a1" 0) (SingleValue 0))
                    Global
              )
              Global
              IntPointer,
            DeclarationSchema
              (ExpressionSchema $ VariableSchema (GlobalVar "_c3" 0) (SingleValue 0))
              ( StatementSchema $
                  AssignmentSchema
                    (ExpressionSchema $ VariableSchema (GlobalVar "_c3" 0) (SingleValue 0))
                    (ExpressionSchema $ DereferenceSchema $ ExpressionSchema $ VariableSchema (GlobalVar "_b2" 0) UntrackedValue)
                    Global
              )
              Global
              IntVar
          ]
    it "Should create a schema for assigning to a derferenced pointer" $
      ( extractSchema $
          ProgramNode
            [ FunctionNode
                IntVar
                "main"
                []
                ( Just $
                    CompoundStmtNode
                      [ DeclarationNode
                          (VarNode "a" mockNodeDat)
                          IntVar
                          Nothing
                          mockNodeDat,
                        PointerNode
                          (VarNode "b" mockNodeDat)
                          IntPointer
                          ( Just $
                              AssignmentNode
                                (VarNode "b" mockNodeDat)
                                (AddressOfNode "a" mockNodeDat)
                                Assignment
                                mockNodeDat
                          )
                          mockNodeDat,
                        AssignDereferenceNode
                          (VarNode "b" mockNodeDat)
                          (ConstantNode 20 mockNodeDat)
                          Assignment
                          mockNodeDat
                      ]
                      mockNodeDat
                )
                mockNodeDat
            ]
      )
        `shouldBe` ProgramSchema
          [ FunctionSchema
              "main"
              ( StatementSchema $
                  CompoundStatementSchema
                    [ DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                        SkipSchema
                        Local
                        IntVar,
                      DeclarationSchema
                        (ExpressionSchema $ VariableSchema (LocalVar (-24) 0 24) UntrackedValue)
                        ( StatementSchema $
                            AssignmentSchema
                              (ExpressionSchema $ VariableSchema (LocalVar (-24) 0 24) UntrackedValue)
                              (ExpressionSchema $ AddressOfSchema $ ExpressionSchema $ VariableSchema (LocalVar (-16) 0 16) UntrackedValue)
                              Local
                        )
                        Local
                        IntPointer,
                      StatementSchema $
                        AssignmentSchema
                          (ExpressionSchema $ VariableSchema (LocalVar (-24) 0 24) UntrackedValue)
                          (ExpressionSchema $ LiteralSchema 20)
                          Local,
                      StatementSchema $ ReturnSchema (ExpressionSchema $ LiteralSchema 0)
                    ]
              )
          ]
