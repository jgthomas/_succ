module Parser.TokConvert where

import Types.Operator
import Types.Tokens (OpTok (..))

tokToAssignOp :: OpTok -> Operator
tokToAssignOp tok =
  case tok of
    EqualSign -> Assignment
    _ -> BinaryOp (tokToBinOp tok)

tokToBinOp :: OpTok -> BinaryOp
tokToBinOp tok =
  case tok of
    PlusSign -> Plus
    MinusSign -> Minus
    Backslash -> Divide
    Asterisk -> Multiply
    Percent -> Modulo
    EqualEqual -> Equal
    BangEqual -> NotEqual
    RightArrow -> GreaterThan
    LeftArrow -> LessThan
    RightArrowEqual -> GThanOrEqu
    LeftArrowEqual -> LThanOrEqu
    PipePipe -> LogicalOR
    AmpAmp -> LogicalAND
    PlusEqual -> Plus
    MinusEqual -> Minus
    AsteriskEqual -> Multiply
    BackslashEqual -> Divide
    PercentEqual -> Modulo
    Caret -> BitwiseXOR
    Ampersand -> BitwiseAND
    Pipe -> BitwiseOR
    CaretEqual -> BitwiseXOR
    AmpEqual -> BitwiseAND
    PipeEqual -> BitwiseOR
    DoubleLeftArrow -> ShiftOp LeftShift
    DoubleRightArrow -> ShiftOp RightShift
    DoubleLArrowEqual -> ShiftOp LeftShift
    DoubleRArrowEqual -> ShiftOp RightShift
    _ -> undefined

tokToUnaryOp :: OpTok -> UnaryOp
tokToUnaryOp tok =
  case tok of
    Bang -> Unary LogicalNeg
    MinusSign -> Unary Negate
    PlusSign -> Unary Positive
    Tilde -> Unary BitwiseComp
    PlusPlus -> PreOpUnary PreIncrement
    MinusMinus -> PreOpUnary PreDecrement
    _ -> undefined

tokToPostUnaryOp :: OpTok -> UnaryOp
tokToPostUnaryOp tok =
  case tok of
    PlusPlus -> PostOpUnary PostIncrement
    MinusMinus -> PostOpUnary PostDecrement
    _ -> undefined
