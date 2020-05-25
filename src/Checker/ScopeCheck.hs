{-|
Module       : ScopeCheck
Description  : Checks for scope errors

Checks for scope errors in variables and function calls
-}
module Checker.ScopeCheck
        (checkIfUsedInScope,
         validateCall,
         checkIfFuncDefined,
         checkIfDefined,
         validateFuncDeclaration,
         checkCountsMatch,
         checkArguments,
         validateGlobalDeclaration,
         variableExists,
         checkGotoJump,
         validatePrevDecGlobal
        ) where


import           Control.Monad   (unless, when)
import           Data.Maybe      (isNothing)

import           State.GenState  (GenState, throwError)
import qualified State.SymTab    as SymTab
import           Types.AST       (Tree (..))
import           Types.Error     (CompilerError (ScopeError), ScopeError (..))
import           Types.Variables (VarLookup (..), VarType (ParamVar))


-- | Throw error if variable name exists in current scope
checkIfUsedInScope :: Tree -> GenState ()
checkIfUsedInScope node@(DeclarationNode (VarNode name _) _ _ _) = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           throwError $ ScopeError (DoubleDeclaredNode node)
checkIfUsedInScope tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if function call sequence is invalid
validateCall :: Tree -> GenState ()
validateCall node@(FuncCallNode name _ _) = do
        callee <- SymTab.decSeqNumber name
        caller <- SymTab.currentSeqNumber
        unless (validSeq callee caller) $
            throwError $ ScopeError (InvalidCallNode node)
validateCall tree = throwError $ ScopeError (UnexpectedNode tree)


validSeq :: Maybe Int -> Maybe Int -> Bool
validSeq Nothing Nothing   = False
validSeq Nothing (Just _)  = False
validSeq (Just _) Nothing  = False
validSeq (Just a) (Just b) = a <= b


-- | Throw error if a function is already defined
checkIfFuncDefined :: Tree -> GenState ()
checkIfFuncDefined node@(FunctionNode _ name _ _ _) = do
        defined <- SymTab.checkFuncDefined name
        when defined $
           throwError $ ScopeError (DoubleDefinedNode node)
checkIfFuncDefined tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if a variable is already defined
checkIfDefined :: Tree -> GenState ()
checkIfDefined node@(AssignmentNode (VarNode name _) _ _ _) = do
        defined <- SymTab.checkVarDefined name
        when defined $
           throwError $ ScopeError (DoubleDefinedNode node)
checkIfDefined tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if declared function identifier is already used
validateFuncDeclaration :: Tree -> GenState ()
validateFuncDeclaration node@(FunctionNode _ name _ _ _) = do
        label <- SymTab.globalLabel name
        unless (isNothing label) $
            throwError $ ScopeError (DoubleDefinedNode node)
validateFuncDeclaration tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if parameter or argument counts do not match
checkCountsMatch :: Int -> Tree -> GenState ()
checkCountsMatch count node@(FunctionNode _ _ paramList _ _) =
        when (count /= length paramList) $
           throwError $ ScopeError (MisMatchNode count node)
checkCountsMatch count node@(FuncCallNode _ argList _) =
        when (count /= length argList) $
           throwError $ ScopeError (MisMatchNode count node)
checkCountsMatch _ tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if number of arguments do not match number of parameters
checkArguments :: Maybe Int -> Tree -> GenState ()
checkArguments (Just n) node@FuncCallNode{} = checkCountsMatch n node
checkArguments (Just _) tree = throwError $ ScopeError (UnexpectedNode tree)
checkArguments Nothing tree  = throwError $ ScopeError (UndeclaredNode tree)


-- | Throw error if global variable identifier already used for function
validateGlobalDeclaration :: Tree -> GenState ()
validateGlobalDeclaration node@(DeclarationNode (VarNode name _) _ _ _) = do
        paramNum <- SymTab.decParamCount name
        unless (isNothing paramNum) $
            throwError $ ScopeError (DoubleDeclaredNode node)
validateGlobalDeclaration tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Throw error if trying to use an indentifier that has not been declared
variableExists :: Tree -> GenState ()
variableExists node@(VarNode a _)         = varExists node a
variableExists node@(AddressOfNode a _)   = addressableVarExists node a
variableExists node@(DereferenceNode a _) = varExists node a
variableExists tree = throwError $ ScopeError (UnexpectedNode tree)


varExists :: Tree -> String -> GenState ()
varExists node name = do
        var <- SymTab.getVariable name
        when (var == NotFound) $
            throwError $ ScopeError (UnrecognisedNode node)


addressableVarExists :: Tree -> String -> GenState ()
addressableVarExists node name = do
        var <- SymTab.getVariable name
        case var of
             VarType ParamVar{} -> throwError $ ScopeError (Unaddressable node)
             _                  -> varExists node name


-- | Throw error if break or continue have not been defined for scope
checkGotoJump :: Tree -> GenState ()
checkGotoJump node@BreakNode{} = do
        breakLabel <- SymTab.getBreak
        when (isNothing breakLabel) $
            throwError $ ScopeError (UnexpectedNode node)
checkGotoJump node@ContinueNode{} = do
        continueLabel <- SymTab.getContinue
        when (isNothing continueLabel) $
            throwError $ ScopeError (UnexpectedNode node)
checkGotoJump node = throwError $ ScopeError (UnexpectedNode node)


validatePrevDecGlobal :: Maybe String -> Tree -> GenState ()
validatePrevDecGlobal Nothing node =
        throwError $ ScopeError (UndeclaredNode node)
validatePrevDecGlobal _ (AssignmentNode _ valNode _ _) =
        throwError $ ScopeError (UnexpectedNode valNode)
validatePrevDecGlobal _ tree =
        throwError $ ScopeError (UnexpectedNode tree)
