
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
              | BitwiseXOR
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
             Caret           -> BitwiseXOR
             _               -> undefined


tokToUnaryOp :: OpTok -> UnaryOp
tokToUnaryOp tok =
        case tok of
             Bang       -> Unary LogicalNeg
             MinusSign  -> Unary Negate
             PlusSign   -> Unary Positive
             Tilde      -> Unary BitwiseComp
             PlusPlus   -> PreOpUnary PreIncrement
             MinusMinus -> PreOpUnary PreDecrement
             _          -> undefined


tokToPostUnaryOp :: OpTok -> UnaryOp
tokToPostUnaryOp tok =
        case tok of
             PlusPlus   -> PostOpUnary PostIncrement
             MinusMinus -> PostOpUnary PostDecrement
             _          -> undefined

