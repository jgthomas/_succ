
module NewOps where


import Tokens


data Oper = UnaryOp UnaryOp
          | BinaryOp BinaryOp
          deriving (Show, Eq)


data BinaryOp = OpPlus
              | OpMinus
              | OpDivide
              | OpMultiply
              | OpModulo
              | OpEqual
              | OpNotEqual
              | OpGrThan
              | OpLeThan
              | OpGrThanOrEq
              | OpLeThanOrEq
              | OpAssign
              | OpLogicOR
              | OpLogicAND
              deriving (Show, Eq)


data UnaryOp = Negative
             | BitComp
             | LogicNegation
             deriving (Show, Eq)


tokToBinOp :: Operator -> BinaryOp
tokToBinOp tok =
        case tok of
             Plus               -> OpPlus
             Minus              -> OpMinus
             Divide             -> OpDivide
             Multiply           -> OpMultiply
             Modulo             -> OpModulo
             Equal              -> OpEqual
             NotEqual           -> OpNotEqual
             GreaterThan        -> OpGrThan
             LessThan           -> OpLeThan
             GreaterThanOrEqual -> OpGrThanOrEq
             LessThanOrEqual    -> OpLeThanOrEq
             Assign             -> OpAssign
             LogicalOR          -> OpLogicOR
             LogicalAND         -> OpLogicAND
             PlusAssign         -> OpPlus
             MinusAssign        -> OpMinus
             MultiplyAssign     -> OpMultiply
             DivideAssign       -> OpDivide
             ModuloAssign       -> OpModulo
             _                  -> undefined
