
module Converter.Analyser (analyse) where


import Compute.ComputeExpression as Compute (constantTrue)
import State.GenState            (GenState)
import Types.AST                 (NodeDat (isSkipped), Tree (..))


analyse :: Tree -> GenState Tree

analyse (CompoundStmtNode trees dat) = do
        checkedTrees <- mapM analyse trees
        pure (CompoundStmtNode checkedTrees dat)

analyse ifNode@(IfNode cond (ExprStmtNode (AssignmentNode a b o dat) d) e d') = do
        condTrue <- conditionTrue cond
        if condTrue
           then pure ifNode
           else pure (IfNode cond (ExprStmtNode (AssignmentNode a b o $ dat { isSkipped = True }) d) e d')

analyse tree = pure tree


conditionTrue :: Tree -> GenState Bool

conditionTrue (ConstantNode n _) = pure $ Compute.constantTrue n

conditionTrue _                  = pure True
