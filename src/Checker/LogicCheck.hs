
module Checker.LogicCheck (checkUnaryLogic, checkAssignLocalLogic) where


import State.GenState (GenState, throwError)
import Types.AST      (Tree (..))
import Types.Error    (CompilerError (LogicError), LogicError (..))
import Types.Operator


checkUnaryLogic :: Tree -> GenState ()
checkUnaryLogic (UnaryNode VarNode{} _ _) = pure ()
checkUnaryLogic (UnaryNode _ (Unary _) _) = pure ()
checkUnaryLogic node@(UnaryNode _ unOp@(PreOpUnary _) _) =
        throwError $ LogicError (OperatorUseError node $ UnaryOp unOp)
checkUnaryLogic node@(UnaryNode _ unOp@(PostOpUnary _) _) =
        throwError $ LogicError (OperatorUseError node $ UnaryOp unOp)
checkUnaryLogic node = throwError $ LogicError (MalformedNode node)


checkAssignLocalLogic :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocalLogic _ _ Assignment                 = pure ()
checkAssignLocalLogic DereferenceNode{} _ BinaryOp{} = pure ()
checkAssignLocalLogic VarNode{} _ BinaryOp{}         = pure ()
checkAssignLocalLogic valNode _ unOp@UnaryOp{} =
        throwError $ LogicError (AssignmentLogicError valNode unOp)
checkAssignLocalLogic node _ _ =
        throwError $ LogicError (AssignmentTreeError node)
