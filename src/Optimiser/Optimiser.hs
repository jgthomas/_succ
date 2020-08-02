
module Optimiser.Optimiser (optimiseExpression) where


import Types.AssemblySchema
import Types.Operator


optimiseExpression :: ExpressionSchema -> ExpressionSchema

optimiseExpression schema@LiteralSchema{} = schema

optimiseExpression schema@BinarySchema{}  = optimiseBinarySchema schema

optimiseExpression schema                 = schema


optimiseBinarySchema :: ExpressionSchema -> ExpressionSchema

optimiseBinarySchema (BinarySchema (LiteralSchema n) (LiteralSchema m) op _ _) =
        LiteralSchema $ (binaryFunction op) n m

optimiseBinarySchema schema = schema


binaryFunction :: Integral a => BinaryOp -> (a -> a -> a)
binaryFunction Plus     = (+)
binaryFunction Minus    = (-)
binaryFunction Multiply = (*)
binaryFunction Divide   = quot
binaryFunction _        = undefined
