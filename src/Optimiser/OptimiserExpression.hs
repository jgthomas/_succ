{-|
Module       : OptimiserExpression
Description  : Optimise expression schema

Optimises expression schema to reduce assembly code size
-}
module Optimiser.OptimiserExpression (optimiseExpression) where


import           Data.Data                 (toConstr)

import qualified Compute.ComputeExpression as ComputeExpression (binaryFunction)
import           Types.AssemblySchema
import           Types.Operator


-- | Optimises an expression schema
optimiseExpression :: ExpressionSchema -> ExpressionSchema
optimiseExpression schema@LiteralSchema{} = schema
optimiseExpression schema@UnarySchema{}   = schema
optimiseExpression schema@BinarySchema{}  = optimiseBinarySchema schema
optimiseExpression schema                 = schema


optimiseBinarySchema :: ExpressionSchema -> ExpressionSchema

optimiseBinarySchema (BinarySchema
                      (ExpressionSchema (LiteralSchema n))
                      (ExpressionSchema (LiteralSchema m))
                      op _ _) =
                              buildLiteral $ ComputeExpression.binaryFunction op n m

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
        | otherwise = UnarySchema
                      (ExpressionSchema (LiteralSchema (abs n)))
                      (Unary Negate)


hasOptimised :: ExpressionSchema -> ExpressionSchema -> Bool
hasOptimised schema1 schema2 = toConstr schema1 /= toConstr schema2
