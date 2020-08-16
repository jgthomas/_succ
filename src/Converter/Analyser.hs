{-|
Module       : Analyser
Description  : Analyses statements

Analyses the logic of statements
-}
module Converter.Analyser (analyse) where


import Compute.ComputeExpression as Compute (constantTrue)
import State.GenState            (GenState)
import Types.AST                 (NodeDat (isSkipped), Tree (..))


-- | Analyse a syntax tree node
analyse :: Tree -> GenState Tree

analyse (CompoundStmtNode trees dat) = do
        checkedTrees <- mapM analyse trees
        pure (CompoundStmtNode checkedTrees dat)

analyse ifNode@(IfNode cond (ExprStmtNode assign@AssignmentNode{} d) e d') = do
        condTrue <- conditionTrue cond
        if condTrue
           then pure ifNode
           else do
                   skippedAssign <- setAsSkipped assign
                   pure (IfNode cond (ExprStmtNode skippedAssign d) e d')

analyse tree = pure tree


setAsSkipped :: Tree -> GenState Tree

setAsSkipped (AssignmentNode a b o dat) = pure (AssignmentNode a b o $ dat { isSkipped = True })

setAsSkipped tree                       = pure tree


conditionTrue :: Tree -> GenState Bool

conditionTrue (ConstantNode n _) = pure $ Compute.constantTrue n

conditionTrue _                  = pure True
