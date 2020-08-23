{-|
Module       : Analyser
Description  : Analyses statements

Analyse the logic of statements, setting metadata properties.
-}
module Converter.Analyser (analyse) where


import qualified Compute.ComputeExpression as Compute (binaryFunction,
                                                       constantTrue,
                                                       unaryFunction)
import           State.GenState            (GenState)
import qualified State.State               as State (getVariableValue)
import           Types.AST                 (NodeDat (isSkipped, notTracked),
                                            Tree (..))
import           Types.Operator
import           Types.Variables           (VarValue (..))


-- | Analyse a syntax tree node
analyse :: Tree -> GenState Tree
analyse ifNode@IfNode{}           = setConditional ifNode
analyse whileNode@WhileNode{}     = pure $ setVarsUntracked whileNode
analyse doWhileNode@DoWhileNode{} = pure $ setVarsUntracked doWhileNode
analyse forLoopNode@ForLoopNode{} = pure $ setVarsUntracked forLoopNode
analyse tree                      = pure tree


setConditional :: Tree -> GenState Tree
setConditional ifNode@(IfNode cond _ _ _) = do
        condTrue <- conditionTrue cond
        if condTrue
           then pure $ setVarsUntracked ifNode
           else pure $ setStatementsSkipped ifNode
setConditional tree = pure tree


setStatementsSkipped :: Tree -> Tree

setStatementsSkipped (IfNode cond (ExprStmtNode assign@AssignmentNode{} d) e d') =
        IfNode cond (ExprStmtNode (setAsSkipped assign) d) e d'

setStatementsSkipped (IfNode cond (CompoundStmtNode statements d) e d') =
        IfNode cond (CompoundStmtNode (map setAsSkipped statements) d) e d'

setStatementsSkipped tree = tree


setVarsUntracked :: Tree -> Tree

setVarsUntracked (WhileNode cond (CompoundStmtNode statements d) d') =
        WhileNode cond (CompoundStmtNode (map setNotTracked statements) d) d'

setVarsUntracked (DoWhileNode (CompoundStmtNode statements d) cond d') =
        DoWhileNode (CompoundStmtNode (map setNotTracked statements) d) cond d'

setVarsUntracked (ForLoopNode ini test iter (CompoundStmtNode statements d) d') =
        ForLoopNode ini test iter (CompoundStmtNode (map setNotTracked statements) d) d'

setVarsUntracked (ForLoopNode ini test iter statement d) =
        ForLoopNode ini test iter (setNotTracked statement) d

setVarsUntracked (IfNode cond (ExprStmtNode assign@AssignmentNode{} d) e d') =
        IfNode cond (ExprStmtNode (setNotTracked assign) d) e d'

setVarsUntracked (IfNode
                  cond
                  (CompoundStmtNode statements d)
                  (Just (CompoundStmtNode elseStatements d'))
                  d''
                 ) = IfNode
                     cond
                     (CompoundStmtNode (map setNotTracked statements) d)
                     (Just $ CompoundStmtNode (map setNotTracked elseStatements) d')
                     d''

setVarsUntracked (IfNode cond (CompoundStmtNode statements d) e d') =
        IfNode cond (CompoundStmtNode (map setNotTracked statements) d) e d'

setVarsUntracked tree = tree


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

setAsSkipped assign@AssignmentNode{} =
        setMetaDataFlag Skipped assign

setAsSkipped (ExprStmtNode unary@UnaryNode{} d') =
        ExprStmtNode (setMetaDataFlag Skipped unary) d'

setAsSkipped tree = tree


setNotTracked :: Tree -> Tree

setNotTracked (ExprStmtNode assign@AssignmentNode{} dat) =
        ExprStmtNode (setMetaDataFlag NotTracked assign) dat

setNotTracked (DeclarationNode varNode typ (Just assign@AssignmentNode{}) dat) =
        DeclarationNode varNode typ (Just $ setMetaDataFlag NotTracked assign) dat

setNotTracked (ExprStmtNode unary@UnaryNode{} d') =
        ExprStmtNode (setMetaDataFlag NotTracked unary) d'

setNotTracked tree = tree


setMetaDataFlag :: Flag -> Tree -> Tree

setMetaDataFlag flag (AssignmentNode l r o dat) =
        AssignmentNode l r o $ metaDataUpdate flag dat

setMetaDataFlag flag (UnaryNode v@VarNode{} op@PreOpUnary{} dat) =
        UnaryNode v op $ metaDataUpdate flag dat

setMetaDataFlag flag (UnaryNode v@VarNode{} op@PostOpUnary{} dat) =
        UnaryNode v op $ metaDataUpdate flag dat

setMetaDataFlag _ tree = tree


metaDataUpdate :: Flag -> NodeDat -> NodeDat
metaDataUpdate Skipped dat    = dat { isSkipped = True }
metaDataUpdate NotTracked dat = dat { notTracked = True }


data Flag = Skipped
          | NotTracked
          deriving (Eq, Show)


isTrue :: Int -> Bool
isTrue 0 = False
isTrue _ = True
