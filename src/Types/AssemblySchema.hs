{-# LANGUAGE DeriveDataTypeable #-}

module Types.AssemblySchema where


import Data.Data       (Data)

import Types.Operator  (BinaryOp, UnaryOp)
import Types.Type      (Type (..))
import Types.Variables (Scope, VarType, VarValue)


data AssemblySchema = ProgramSchema
                       [AssemblySchema]
                    | FunctionSchema
                       String
                       AssemblySchema
                    | DeclarationSchema
                       AssemblySchema
                       AssemblySchema
                       Scope
                       Type
                    | StatementSchema
                       StatementSchema
                    | ExpressionSchema
                       ExpressionSchema
                    | SkipSchema
                    deriving (Eq, Show, Data)


data StatementSchema = IfSchema
                        AssemblySchema
                        AssemblySchema
                        AssemblySchema
                        Label
                        Label
                     | ForSchema
                        AssemblySchema
                        AssemblySchema
                        AssemblySchema
                        AssemblySchema
                        Label
                        Label
                        Label
                     | WhileSchema
                        AssemblySchema
                        AssemblySchema
                        Label
                        Label
                     | DoWhileSchema
                        AssemblySchema
                        AssemblySchema
                        Label
                        Label
                        Label
                     | ContinueSchema
                        Label
                     | BreakSchema
                        Label
                     | ReturnSchema
                        AssemblySchema
                     | AssignmentSchema
                        AssemblySchema
                        AssemblySchema
                        Scope
                     | CompoundStatementSchema
                        [AssemblySchema]
                     | ArrayItemsSchema
                        Int
                        [AssemblySchema]
                     deriving (Eq, Show, Data)


data ExpressionSchema = LiteralSchema
                         Int
                      | VariableSchema
                         VarType
                         VarValue
                      | DereferenceSchema
                         AssemblySchema
                      | AddressOfSchema
                         AssemblySchema
                      | FunctionCallSchema
                         String
                         [AssemblySchema]
                      | UnarySchema
                         AssemblySchema
                         UnaryOp
                      | BinarySchema
                         AssemblySchema
                         AssemblySchema
                         BinaryOp
                         Label
                         Label
                      | TernarySchema
                         AssemblySchema
                         AssemblySchema
                         AssemblySchema
                         Label
                         Label
                      deriving (Eq, Show, Data)


data Label = LocalLabel Int
           | GlobalLabel String
           deriving (Eq, Show, Data)
