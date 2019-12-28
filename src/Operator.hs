
module Operator where


import Tokens (OpTok (..))


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
              | Assignment
              | LogicalOR
              | LogicalAND
              deriving (Show, Eq)


data UnaryOp = Negate
             | BitwiseComp
             | LogicalNeg
             | Increment
             | Decrement
             | Positive
             deriving (Show, Eq)


tokToBinOp :: OpTok -> BinaryOp
tokToBinOp tok =
        case tok of
             PlusSign        -> Plus
             MinusSign       -> Minus
             Backslash       -> Divide
             Asterisk        -> Multiply
             Percent         -> Modulo
             EqualEqual      -> Equal
             BangEqual       -> NotEqual
             RightArrow      -> GreaterThan
             LeftArrow       -> LessThan
             RightArrowEqual -> GThanOrEqu
             LeftArrowEqual  -> LThanOrEqu
             EqualSign       -> Assignment
             PipePipe        -> LogicalOR
             AmpAmp          -> LogicalAND
             PlusEqual       -> Plus
             MinusEqual      -> Minus
             AsteriskEqual   -> Multiply
             BackslashEqual  -> Divide
             PercentEqual    -> Modulo
             _               -> undefined


tokToUnaryOp :: OpTok -> UnaryOp
tokToUnaryOp tok =
        case tok of
             Bang       -> LogicalNeg
             MinusSign  -> Negate
             Tilde      -> BitwiseComp
             PlusPlus   -> Increment
             MinusMinus -> Decrement
             PlusSign   -> Positive
             _          -> undefined
