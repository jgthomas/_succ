
module Optimiser.Optimiser (optimiseExpression) where


import Data.Data            (toConstr)

import Types.AssemblySchema
import Types.Operator


optimiseExpression :: ExpressionSchema -> ExpressionSchema

optimiseExpression schema@LiteralSchema{} = schema

optimiseExpression schema@UnarySchema{} = schema

optimiseExpression schema@(BinarySchema _ _ op _ _) =
        if op `elem` supported
           then optimiseBinarySchema schema
           else schema

optimiseExpression schema = schema


optimiseBinarySchema :: ExpressionSchema -> ExpressionSchema

optimiseBinarySchema (BinarySchema (LiteralSchema n) (LiteralSchema m) op _ _) =
        buildLiteral $ binaryFunction op n m

optimiseBinarySchema schema@(BinarySchema left right op l1 l2) =
        let leftOptimised  = optimiseExpression left
            rightOptimised = optimiseExpression right
                in
        if hasOptimised left leftOptimised || hasOptimised right rightOptimised
           then optimiseExpression (BinarySchema leftOptimised rightOptimised op l1 l2)
           else schema

optimiseBinarySchema schema = schema


buildLiteral :: Int -> ExpressionSchema
buildLiteral n
        | n >= 0    = LiteralSchema n
        | otherwise = UnarySchema (LiteralSchema $ abs n) (Unary Negate)


binaryFunction :: Integral a => BinaryOp -> (a -> a -> a)
binaryFunction Plus     = (+)
binaryFunction Minus    = (-)
binaryFunction Multiply = (*)
binaryFunction Divide   = quot
binaryFunction Modulo   = mod
binaryFunction _        = undefined


supported :: [BinaryOp]
supported = [
        Plus,
        Minus,
        Multiply,
        Divide,
        Modulo
       ]


hasOptimised :: ExpressionSchema -> ExpressionSchema -> Bool
hasOptimised schema1 schema2 = toConstr schema1 /= toConstr schema2
