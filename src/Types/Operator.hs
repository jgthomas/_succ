
module Types.Operator where


data Operator = Assignment
              | BinaryOp BinaryOp
              | UnaryOp UnaryOp
              deriving (Show, Eq)


data BinaryOp = Plus
              | Minus
              | Divide
              | Multiply
              | Modulo
              | Equal
              | NotEqual
              | GreaterThan
              | LessThan
              | GThanOrEqu
              | LThanOrEqu
              | LogicalOR
              | LogicalAND
              | BitwiseXOR
              | BitwiseAND
              | BitwiseOR
              | ShiftOp ShiftOp
              deriving (Show, Eq)


data ShiftOp = LeftShift
             | RightShift
             deriving (Show, Eq)


data UnaryOp = Unary Unary
             | PreOpUnary PreOpUnary
             | PostOpUnary PostOpUnary
             deriving (Show, Eq)


data Unary = Negate
           | Positive
           | BitwiseComp
           | LogicalNeg
           deriving (Show, Eq)


data PreOpUnary = PreIncrement
                | PreDecrement
                deriving (Show, Eq)


data PostOpUnary = PostIncrement
                 | PostDecrement
                 deriving (Show, Eq)
