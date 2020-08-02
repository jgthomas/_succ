
module Optimiser.Optimiser (optimiseExpression) where


import Types.AssemblySchema
import Types.Operator


optimiseExpression :: ExpressionSchema -> ExpressionSchema

optimiseExpression schema@LiteralSchema{} = schema

optimiseExpression schema@BinarySchema{}  = optimiseBinarySchema schema

optimiseExpression schema                 = schema


optimiseBinarySchema :: ExpressionSchema -> ExpressionSchema

optimiseBinarySchema schema@(BinarySchema (LiteralSchema n) (LiteralSchema m) op _ _) =
        if op `elem` supported
           then LiteralSchema $ binaryFunction op n m
           else schema

optimiseBinarySchema schema = schema


binaryFunction :: Integral a => BinaryOp -> (a -> a -> a)
binaryFunction Plus     = (+)
binaryFunction Minus    = (-)
binaryFunction Multiply = (*)
binaryFunction Divide   = quot
binaryFunction _        = undefined


supported :: [BinaryOp]
supported = [
        Plus,
        Minus,
        Multiply,
        Divide
       ]
