-- |
-- Module       : Valuer
-- Description  : Evaluates tree values
--
-- Determines the value to be tracked in the state for a node in the syntax tree
module Analyser.Valuer
  ( value,
  )
where

import Compute.ComputeExpression as Compute (binaryFunction)
import State.GenState (GenState)
import State.State as State (getVariableValue)
import Types.AST (Tree (..))
import Types.Operator
import Types.Variables (VarValue (..))

-- | Determine a tree node value
value :: Tree -> GenState VarValue
value (ConstantNode n _) = pure $ SingleValue n
value (VarNode name _) = State.getVariableValue name
value (BinaryNode left right op _) = do
  leftValue <- value left
  rightValue <- value right
  combineBinary op leftValue rightValue
value (ArgNode valNode _) = value valNode
value _ = pure UntrackedValue

combineBinary :: BinaryOp -> VarValue -> VarValue -> GenState VarValue
combineBinary op (SingleValue n) (SingleValue m) = pure $ SingleValue (Compute.binaryFunction op n m)
combineBinary _ _ _ = pure UntrackedValue
