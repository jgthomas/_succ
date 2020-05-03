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
         checkIfVariable,
         checkCountsMatch,
         checkArguments,
         checkIfFunction,
         variableExists
        ) where


import           Control.Monad   (unless, when)
import           Data.Maybe      (isNothing)

import           Error.Error     (CompilerError (ScopeError), ScopeError (..))
import           State.GenState  (GenState, throwError)
import qualified State.SymTab    as SymTab
import           Types.AST       (Tree (..))
import           Types.Variables (VarLookup (..), VarType (ParamVar))


-- | Check if variable name exists in current scope
checkIfUsedInScope :: Tree -> GenState ()
checkIfUsedInScope node@(DeclarationNode (VarNode name _) _ _ _) = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           throwError $ ScopeError (DoubleDeclaredNode node)
checkIfUsedInScope tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Validate a function call sequence
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


-- | Check if a function is already defined
checkIfFuncDefined :: Tree -> GenState ()
checkIfFuncDefined node@(FunctionNode _ name _ _ _) = do
        defined <- SymTab.checkFuncDefined name
        when defined $
           throwError $ ScopeError (DoubleDefinedNode node)
checkIfFuncDefined tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Check if a variable has been defined
checkIfDefined :: Tree -> GenState ()
checkIfDefined node@(AssignmentNode (VarNode name _) _ _ _) = do
        defined <- SymTab.checkVarDefined name
        when defined $
           throwError $ ScopeError (DoubleDefinedNode node)
checkIfDefined tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Check if an identifier is a variable
checkIfVariable :: Tree -> GenState ()
checkIfVariable node@(FunctionNode _ name _ _ _) = do
        label <- SymTab.globalLabel name
        unless (isNothing label) $
            throwError $ ScopeError (DoubleDefinedNode node)
checkIfVariable tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Check parameter counts match in two function declarations
checkCountsMatch :: Int -> Tree -> GenState ()
checkCountsMatch count node@(FunctionNode _ _ paramList _ _) =
        when (count /= length paramList) $
           throwError $ ScopeError (MisMatchNode count node)
checkCountsMatch count node@(FuncCallNode _ argList _) =
        when (count /= length argList) $
           throwError $ ScopeError (MisMatchNode count node)
checkCountsMatch _ tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Check argument counts match in two function calls
checkArguments :: Maybe Int -> Tree -> GenState ()
checkArguments (Just n) node@FuncCallNode{} = checkCountsMatch n node
checkArguments (Just _) tree = throwError $ ScopeError (UnexpectedNode tree)
checkArguments Nothing tree  = throwError $ ScopeError (UndeclaredNode tree)


-- | Check if an identifier is a function
checkIfFunction :: Tree -> GenState ()
checkIfFunction node@(DeclarationNode (VarNode name _) _ _ _) = do
        paramNum <- SymTab.decParamCount name
        unless (isNothing paramNum) $
            throwError $ ScopeError (DoubleDeclaredNode node)
checkIfFunction tree = throwError $ ScopeError (UnexpectedNode tree)


-- | Check an identifier is linked to real variable
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

