
module NewOps where


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


data UnaryOp = Negative
             | BitComp
             | LogicNeg
             deriving (Show, Eq)


tokToBinOp :: OpTok -> BinaryOp
tokToBinOp tok =
        case tok of
             PlusSign         -> Plus
             MinusSign        -> Minus
             BackSlash        -> Divide
             Asterisk         -> Multiply
             Percent          -> Modulo
             EqualEqual       -> Equal
             BangEqual        -> NotEqual
             RightArrow       -> GreaterThan
             LeftArrow        -> LessThan
             RightArrowEquals -> GThanOrEqu
             LeftArrowEquals  -> LThanOrEqu
             EqualSign        -> Assignment
             PipePipe         -> LogicalOR
             AmpAmp           -> LogicalAND
             PlusEqual        -> Plus
             MinusEqual       -> Minus
             AsteriskEqual    -> Multiply
             BackSlashEqual   -> Divide
             PercentEqual     -> Modulo
             _                -> undefined


tokToUnaryOp :: OpTok -> UnaryOp
tokToUnaryOp tok =
        case tok of
             Bang      -> LogicNeg
             MinusSign -> Negative
             Tilde     -> BitComp
             _         -> undefined
