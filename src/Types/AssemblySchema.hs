
module Types.AssemblySchema where


data AssemblySchema = StatementSchema StatementSchema
                    | ExpressionSchema ExpressionSchema


data StatementSchema = IfSchema
                     | ForSchema
                     | WhileSchema
                     | DoWhileSchema
                     | ReturnSchema ExpressionSchema


data ExpressionSchema = Literal Int
                      | Variable String
                      | Unary
                      | Binary
                      | Ternary
