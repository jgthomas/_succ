{-|
Module       : ComputeExpression
Description  : Computes expression values

Computes the values of expression schemas
-}
module Compute.ComputeExpression where


import Types.Operator


-- | Matches binary operator to Haskell function
binaryFunction :: Integral a => BinaryOp -> (a -> a -> a)
binaryFunction Plus        = (+)
binaryFunction Minus       = (-)
binaryFunction Multiply    = (*)
binaryFunction Divide      = quot
binaryFunction Modulo      = mod
binaryFunction Equal       = \x y -> if x == y then 1 else 0
binaryFunction NotEqual    = \x y -> if x /= y then 1 else 0
binaryFunction GreaterThan = \x y -> if x > y then 1 else 0
binaryFunction LessThan    = \x y -> if x < y then 1 else 0
binaryFunction GThanOrEqu  = \x y -> if x >= y then 1 else 0
binaryFunction LThanOrEqu  = \x y -> if x <= y then 1 else 0
binaryFunction _           = undefined
