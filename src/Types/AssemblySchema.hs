
module Types.AssemblySchema where


import Types.Operator
import Types.Variables (Scope (..))


data AssemblySchema = ProgramSchema
                       [AssemblySchema]
                    | FunctionSchema
                       String
                       AssemblySchema
                    | DeclarationSchema
                       AssemblySchema
                       (Maybe AssemblySchema)
                    | StatementSchema
                       StatementSchema
                    | ExpressionSchema
                       ExpressionSchema
                    | SkipSchema
                    deriving (Eq, Show)


data StatementSchema = IfSchema
                     | ForSchema
                     | WhileSchema
                        ExpressionSchema
                        StatementSchema
                        Label
                        Label
                     | DoWhileSchema
                        StatementSchema
                        ExpressionSchema
                        Label
                        Label
                        Label
                     | ContinueSchema
                        Label
                     | BreakSchema
                        Label
                     | ReturnSchema
                        ExpressionSchema
                     | CompoundStatementSchema
                        [AssemblySchema]
                     deriving (Eq, Show)


data ExpressionSchema = LiteralSchema
                         Int
                         Scope
                      | VariableSchema
                         String
                      | UnarySchema
                         ExpressionSchema
                         UnaryOp
                      | BinarySchema
                         ExpressionSchema
                         ExpressionSchema
                         BinaryOp
                         String
                         String
                      | TernarySchema
                         ExpressionSchema
                         ExpressionSchema
                         ExpressionSchema
                         Label
                      deriving (Eq, Show)


data Label = LocalLabel Int
           | GlobalLabel String
           deriving (Eq, Show)
