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
           else pure (IfNode cond (ExprStmtNode (setAsSkipped assign) d) e d')

analyse tree = pure tree


setAsSkipped :: Tree -> Tree
setAsSkipped (AssignmentNode l r o dat) = AssignmentNode l r o $ dat { isSkipped = True }
setAsSkipped tree                       = tree


conditionTrue :: Tree -> GenState Bool
conditionTrue (ConstantNode n _) = pure $ isTrue . Compute.constantTrue $ n
conditionTrue _                  = pure True


isTrue :: Int -> Bool
isTrue 0 = False
isTrue _ = True
