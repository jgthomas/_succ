-- |
-- Module       : LogicCheck
-- Description  : Checks for logic errors
--
-- Provides general logic checking capabilities.
module Converter.LogicCheck
  ( validateNode,
  )
where

import State.GenState (GenState, throwError)
import Types.AST (Tree (..))
import Types.Error (CompilerError (LogicError), LogicError (..))
import Types.Operator

-- | Throw error if node is not valid
validateNode :: Tree -> GenState ()
validateNode node@UnaryNode {} = validateUnaryNode node
validateNode node@AssignmentNode {} = validateAssignmentNode node
validateNode _ = pure ()

validateAssignmentNode :: Tree -> GenState ()
validateAssignmentNode (AssignmentNode varNode valNode op _) =
  checkAssignLocalLogic varNode valNode op
validateAssignmentNode _ = pure ()

validateUnaryNode :: Tree -> GenState ()
validateUnaryNode node@UnaryNode {} = checkUnaryLogic node
validateUnaryNode _ = pure ()

checkUnaryLogic :: Tree -> GenState ()
checkUnaryLogic (UnaryNode VarNode {} _ _) = pure ()
checkUnaryLogic (UnaryNode _ (Unary _) _) = pure ()
checkUnaryLogic node@(UnaryNode _ unOp@(PreOpUnary _) _) =
  throwError $ LogicError (OperatorUseError node $ UnaryOp unOp)
checkUnaryLogic node@(UnaryNode _ unOp@(PostOpUnary _) _) =
  throwError $ LogicError (OperatorUseError node $ UnaryOp unOp)
checkUnaryLogic node = throwError $ LogicError (MalformedNode node)

checkAssignLocalLogic :: Tree -> Tree -> Operator -> GenState ()
checkAssignLocalLogic _ _ Assignment = pure ()
checkAssignLocalLogic DereferenceNode {} _ BinaryOp {} = pure ()
checkAssignLocalLogic VarNode {} _ BinaryOp {} = pure ()
checkAssignLocalLogic valNode _ unOp@UnaryOp {} =
  throwError $ LogicError (AssignmentLogicError valNode unOp)
checkAssignLocalLogic node _ _ =
  throwError $ LogicError (AssignmentTreeError node)
