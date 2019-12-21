
module NewOps where


import Tokens


data Oper = UnaryOp UnaryOp
          | BinaryOp BinaryOp
          deriving (Show, Eq)


data BinaryOp = Add
              | Sub
              | Div
              | Mul
              | Mod
              | Equality
              | NotEqu
              | GrThan
              | LeThan
              | GrThanOrEqu
              | LeThanOrEqu
              | Assignment
              | LogicOR
              | LogicAND
              deriving (Show, Eq)


data UnaryOp = Negative
             | BitComp
             | LogicNeg
             deriving (Show, Eq)


tokToBinOp :: Operator -> BinaryOp
tokToBinOp tok =
        case tok of
             PlusSign           -> Add
             MinusSign          -> Sub
             BackSlash          -> Div
             Asterisk           -> Mul
             Percent            -> Mod
             Equal              -> Equality
             NotEqual           -> NotEqu
             GreaterThan        -> GrThan
             LessThan           -> LeThan
             GreaterThanOrEqual -> GrThanOrEqu
             LessThanOrEqual    -> LeThanOrEqu
             Assign             -> Assignment
             LogicalOR          -> LogicOR
             LogicalAND         -> LogicAND
             PlusAssign         -> Add
             MinusAssign        -> Sub
             MultiplyAssign     -> Mul
             DivideAssign       -> Div
             ModuloAssign       -> Mod
             _                  -> undefined


tokToUnaryOp :: Operator -> UnaryOp
tokToUnaryOp tok =
        case tok of
             MinusSign     -> Negative
             Tilde         -> BitComp
             LogicNegation -> LogicNeg
             _             -> undefined
