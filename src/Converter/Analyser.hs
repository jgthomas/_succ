{-|
Module       : Analyser
Description  : Analyses statements

Analyses the logic of statements
-}
module Converter.Analyser (analyse) where


import qualified Compute.ComputeExpression as Compute (binaryFunction,
                                                       constantTrue,
                                                       unaryFunction)
import           State.GenState            (GenState)
import qualified State.State               as State (getVariableValue)
import           Types.AST                 (NodeDat (inLoop, isSkipped),
                                            Tree (..))
import           Types.Variables           (VarValue (..))


-- | Analyse a syntax tree node
analyse :: Tree -> GenState Tree

analyse ifNode@(IfNode cond (ExprStmtNode assign@AssignmentNode{} d) e d') = do
        condTrue <- conditionTrue cond
        if condTrue
           then pure ifNode
           else pure (IfNode cond (ExprStmtNode (setAsSkipped assign) d) e d')

analyse (WhileNode cond (CompoundStmtNode statements d) d') =
        pure (WhileNode cond (CompoundStmtNode (map setInLoop statements) d) d')

analyse (DoWhileNode (CompoundStmtNode statements d) cond d') =
        pure (DoWhileNode (CompoundStmtNode (map setInLoop statements) d) cond d')

analyse tree = pure tree


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


setAsSkipped :: Tree -> Tree
setAsSkipped (AssignmentNode l r o dat) = AssignmentNode l r o $ dat { isSkipped = True }
setAsSkipped tree                       = tree


setInLoop :: Tree -> Tree
setInLoop (ExprStmtNode assign@AssignmentNode{} dat) =
        ExprStmtNode (inALoop assign) dat
setInLoop (DeclarationNode varNode typ (Just assign@AssignmentNode{}) dat) =
        DeclarationNode varNode typ (Just $ inALoop assign) dat
setInLoop tree = tree


inALoop :: Tree -> Tree
inALoop (AssignmentNode l r o dat) = AssignmentNode l r o $ dat { inLoop = True }
inALoop tree                       = tree


isTrue :: Int -> Bool
isTrue 0 = False
isTrue _ = True
