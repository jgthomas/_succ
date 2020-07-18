
module Types.AssemblySchema where


import Types.Operator
import Types.Variables


data AssemblySchema = ProgramSchema
                       [AssemblySchema]
                    | FunctionSchema
                       String
                       AssemblySchema
                    | DeclarationSchema
                       AssemblySchema
                       AssemblySchema
                       Scope
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
                     | AssignmentSchema
                        ExpressionSchema
                        ExpressionSchema
                        Scope
                     | CompoundStatementSchema
                        [AssemblySchema]
                     deriving (Eq, Show)


data ExpressionSchema = LiteralSchema
                         Int
                      | VariableSchema
                         VarType
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
