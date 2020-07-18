
module Types.AssemblySchema where


import Types.Operator


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
                        Int
                        Int
                     | DoWhileSchema
                     | ReturnSchema
                        ExpressionSchema
                     | CompoundStatementSchema
                        [AssemblySchema]
                     deriving (Eq, Show)


data ExpressionSchema = LiteralSchema Int
                      | VariableSchema String
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
                         String
                         String
                      deriving (Eq, Show)
