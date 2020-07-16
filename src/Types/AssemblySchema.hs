
module Types.AssemblySchema where


data AssemblySchema = ProgramSchema [AssemblySchema]
                    | FunctionSchema String [AssemblySchema]
                    | DeclarationSchema AssemblySchema (Maybe AssemblySchema)
                    | StatementSchema StatementSchema
                    | ExpressionSchema ExpressionSchema
                    deriving (Eq, Show)


data StatementSchema = IfSchema
                     | ForSchema
                     | WhileSchema
                     | DoWhileSchema
                     | ReturnSchema ExpressionSchema
                     deriving (Eq, Show)


data ExpressionSchema = LiteralSchema Int
                      | VariableSchema String
                      | UnarySchema
                      | BinarySchema
                      | TernarySchema
                      deriving (Eq, Show)
