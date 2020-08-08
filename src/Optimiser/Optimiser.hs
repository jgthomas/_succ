{-|
Module       : Optimiser
Description  : Optimise assembly schema

Optimises expressions in an assembly schema
-}
module Optimiser.Optimiser (optimise) where


import Data.Data            (toConstr)

import Types.AssemblySchema
import Types.Operator


-- | Optimises an assembly schema
optimise :: AssemblySchema -> AssemblySchema
optimise (StatementSchema schema)  = StatementSchema (optimiseStatement schema)
optimise (ExpressionSchema schema) = ExpressionSchema (optimiseExpression schema)
optimise schema                    = schema


optimiseStatement :: StatementSchema -> StatementSchema
optimiseStatement schema = schema


optimiseExpression :: ExpressionSchema -> ExpressionSchema

optimiseExpression schema@LiteralSchema{} = schema

optimiseExpression schema@UnarySchema{} = schema

optimiseExpression schema@(BinarySchema _ _ op _ _) =
        if op `elem` supported
           then optimiseBinarySchema schema
           else schema

optimiseExpression schema = schema


optimiseBinarySchema :: ExpressionSchema -> ExpressionSchema

optimiseBinarySchema (BinarySchema
                      (ExpressionSchema (LiteralSchema n))
                      (ExpressionSchema (LiteralSchema m))
                      op _ _) =
                              buildLiteral $ binaryFunction op n m

optimiseBinarySchema schema@(BinarySchema (ExpressionSchema left) (ExpressionSchema right) op l1 l2) =
        let leftOptim  = optimiseExpression left
            rightOptim = optimiseExpression right
                in
        if hasOptimised left leftOptim || hasOptimised right rightOptim
           then optimiseExpression (BinarySchema
                                    (ExpressionSchema leftOptim)
                                    (ExpressionSchema rightOptim)
                                    op l1 l2)
           else schema

optimiseBinarySchema schema = schema


buildLiteral :: Int -> ExpressionSchema
buildLiteral n
        | n >= 0    = LiteralSchema n
        | otherwise = UnarySchema (ExpressionSchema (LiteralSchema (abs n))) (Unary Negate)


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
