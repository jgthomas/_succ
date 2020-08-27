{-|
Module       : Analyser
Description  : Analyses statements

Analyse the logic of statements, setting metadata properties.
-}
module Converter.Analyser (analyse) where


import qualified Converter.AnalyserBool as AnalyserBool (conditionTrue)
import           State.GenState         (GenState)
import           Types.AST              (NodeDat (isSkipped, notTracked),
                                         Tree (..))
import           Types.Operator


-- | Analyse a syntax tree node
analyse :: Tree -> GenState Tree
analyse ifNode@IfNode{}           = setConditional ifNode
analyse whileNode@WhileNode{}     = pure $ setVarsUntracked whileNode
analyse doWhileNode@DoWhileNode{} = pure $ setVarsUntracked doWhileNode
analyse forLoopNode@ForLoopNode{} = pure $ setVarsUntracked forLoopNode
analyse tree                      = pure tree


setConditional :: Tree -> GenState Tree
setConditional ifNode@(IfNode cond _ _ _) = do
        condTrue <- AnalyserBool.conditionTrue cond
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
        ForLoopNode
        (setNotTracked ini)
        (setNotTracked test)
        (setNotTracked iter)
        (CompoundStmtNode (map setNotTracked statements) d) d'

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


setAsSkipped :: Tree -> Tree

setAsSkipped assign@AssignmentNode{} =
        setMetaDataFlag Skipped assign

setAsSkipped (ExprStmtNode unary@UnaryNode{} d') =
        ExprStmtNode (setMetaDataFlag Skipped unary) d'

setAsSkipped tree = tree


setNotTracked :: Tree -> Tree

setNotTracked (ExprStmtNode assign@AssignmentNode{} dat) =
        ExprStmtNode (setMetaDataFlag NotTracked assign) dat

setNotTracked assign@AssignmentNode{} =
        setMetaDataFlag NotTracked assign

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
