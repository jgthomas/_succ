
module Converter.Checker (check) where


import qualified Converter.LogicCheck as LogicCheck
import qualified Converter.ScopeCheck as ScopeCheck
import qualified Converter.TypeCheck  as TypeCheck
import           State.GenState       (GenState)
import qualified State.GlobalState    as GlobalState
import           Types.AST            (Tree (..))


check :: Tree -> GenState ()

check node@(FunctionNode _ _ _ Nothing _) =
        checkFunction node

check node@(FunctionNode _ _ _ (Just _) _) = do
        ScopeCheck.checkIfFuncDefined node
        checkFunction node

check node@(FuncCallNode name _ _) = do
        paramCount <- GlobalState.decParamCount name
        ScopeCheck.checkArguments paramCount node
        TypeCheck.typesMatch node
        ScopeCheck.validateCall node

check node@(AssignmentNode varNode@VarNode{} valNode@VarNode{} _ _) =
        checkAssignment node varNode valNode

check node@(AssignmentNode varNode@VarNode{} valNode@AddressOfNode{} _ _) =
        checkAssignment node varNode valNode

check node@(AssignmentNode varNode@VarNode{} valNode@DereferenceNode{} _ _) =
        checkAssignment node varNode valNode

check node@(AssignmentNode varNode _ _ _) = do
        ScopeCheck.variableExists varNode
        TypeCheck.assignment node

check node@(UnaryNode varNode@VarNode{} _ _) = do
        ScopeCheck.variableExists varNode
        LogicCheck.checkUnaryLogic node

check node@UnaryNode{} =
        LogicCheck.checkUnaryLogic node

check node@BreakNode{} =
        ScopeCheck.checkGotoJump node

check node@ContinueNode{} =
        ScopeCheck.checkGotoJump node

check node@VarNode{} =
        ScopeCheck.variableExists node

check node@DereferenceNode{} =
        ScopeCheck.variableExists node

check node@AddressOfNode{} =
        ScopeCheck.variableExists node

check _ = pure ()


checkFunction :: Tree -> GenState ()
checkFunction node@(FunctionNode _ funcName _ _ _) = do
        ScopeCheck.validateFuncDeclaration node
        prevParamCount <- GlobalState.decParamCount funcName
        case prevParamCount of
             Nothing -> pure ()
             Just n  -> do
                     ScopeCheck.checkParameters n node
                     TypeCheck.typesMatch node
                     TypeCheck.funcDeclaration node
checkFunction _ = pure ()


checkAssignment :: Tree -> Tree -> Tree -> GenState ()
checkAssignment assign varNode valNode = do
        ScopeCheck.variableExists varNode
        ScopeCheck.variableExists valNode
        TypeCheck.assignment assign
