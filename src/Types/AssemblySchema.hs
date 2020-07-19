
module Types.AssemblySchema where


import Types.Operator
import Types.Type
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
                       Type
                    | StatementSchema
                       StatementSchema
                    | ExpressionSchema
                       ExpressionSchema
                    | SkipSchema
                    deriving (Eq, Show)


data StatementSchema = IfSchema
                        ExpressionSchema
                        StatementSchema
                        AssemblySchema
                        Label
                        Label
                     | ForSchema
                        AssemblySchema
                        ExpressionSchema
                        ExpressionSchema
                        StatementSchema
                        Label
                        Label
                        Label
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
                      | DereferenceSchema
                         ExpressionSchema
                      | AddressOfSchema
                         ExpressionSchema
                      | ArgumentSchema
                         ExpressionSchema
                      | FunctionCallSchema
                        String
                        [ExpressionSchema]
                      | UnarySchema
                         ExpressionSchema
                         UnaryOp
                      | BinarySchema
                         ExpressionSchema
                         ExpressionSchema
                         BinaryOp
                         Label
                         Label
                      | TernarySchema
                         ExpressionSchema
                         ExpressionSchema
                         ExpressionSchema
                         Label
                      | ArrayItemsSchema
                         Int
                         [StatementSchema]
                      deriving (Eq, Show)


data Label = LocalLabel Int
           | GlobalLabel String
           deriving (Eq, Show)
