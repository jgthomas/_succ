{-|
Module       : AnalyserBool
Description  : Analyse Boolean values

Analyse the truth value of Boolean tree nodes.
-}
module Analyser.AnalyserBool (conditionTrue) where


import qualified Compute.ComputeExpression as Compute (binaryFunction,
                                                       constantTrue,
                                                       unaryFunction)
import           State.GenState            (GenState)
import qualified State.State               as State (getVariableValue)
import           Types.AST                 (Tree (..))
import           Types.Variables           (VarValue (..))


-- | Analyse the Boolean value of a tree node
conditionTrue :: Tree -> GenState Bool

conditionTrue (ConstantNode n _) =
        pure $ isTrue . Compute.constantTrue $ n

conditionTrue (VarNode name _) = do
        varValue <- State.getVariableValue name
        case varValue of
             (SingleValue n) -> pure $ isTrue $ Compute.constantTrue n
             _               -> pure True

conditionTrue (UnaryNode (ConstantNode n _) op _) =
        pure $ isTrue $ Compute.unaryFunction op n

conditionTrue (UnaryNode (VarNode name _) op _) = do
        varValue <- State.getVariableValue name
        case varValue of
             (SingleValue n) -> pure $ isTrue $ Compute.unaryFunction op n
             _               -> pure True

conditionTrue (BinaryNode (ConstantNode n _) (ConstantNode m _) op _) =
        pure $ isTrue $ Compute.binaryFunction op n m

conditionTrue _ = pure True


isTrue :: Int -> Bool
isTrue 0 = False
isTrue _ = True
