{-|
Module       : ComputeExpression
Description  : Computes expression values

Computes the values of expression schemas
-}
module Compute.ComputeExpression where


import Data.Bits

import Types.Operator


-- | Calculate result of a binary operation
binaryFunction :: (Bits a, Integral a) => BinaryOp -> (a -> a -> a)
binaryFunction Plus                 = (+)
binaryFunction Minus                = (-)
binaryFunction Multiply             = (*)
binaryFunction Divide               = quot
binaryFunction Modulo               = mod
binaryFunction Equal                = \x y -> if x == y then 1 else 0
binaryFunction NotEqual             = \x y -> if x /= y then 1 else 0
binaryFunction GreaterThan          = \x y -> if x > y then 1 else 0
binaryFunction LessThan             = \x y -> if x < y then 1 else 0
binaryFunction GThanOrEqu           = \x y -> if x >= y then 1 else 0
binaryFunction LThanOrEqu           = \x y -> if x <= y then 1 else 0
binaryFunction LogicalOR            = \x y -> if x > 0 || y > 0 then 1 else 0
binaryFunction LogicalAND           = \x y -> if x > 0 && y > 0 then 1 else 0
binaryFunction BitwiseXOR           = xor
binaryFunction BitwiseAND           = (.&.)
binaryFunction BitwiseOR            = (.|.)
binaryFunction (ShiftOp LeftShift)  = \x y -> shiftL x (fromIntegral y)
binaryFunction (ShiftOp RightShift) = \x y -> shiftR x (fromIntegral y)


-- | Calculate the result of a unary operation
unaryFunction :: (Bits a, Num a) => UnaryOp -> (a -> a)
unaryFunction (Unary Negate)              = negate
unaryFunction (Unary Positive)            = abs
unaryFunction (Unary BitwiseComp)         = complement
unaryFunction (Unary LogicalNeg)          = \x -> if x == 0 then 1 else 0
unaryFunction (PreOpUnary PreIncrement)   = (+1)
unaryFunction (PreOpUnary PreDecrement)   = subtract 1
unaryFunction (PostOpUnary PostIncrement) = (+1)
unaryFunction (PostOpUnary PostDecrement) = subtract 1


-- | Constant to bool conversion
constantTrue :: Int -> Bool
constantTrue 0 = False
constantTrue _ = True
