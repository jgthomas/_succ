
module Checker.LogicCheck (checkUnaryLogic) where


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
