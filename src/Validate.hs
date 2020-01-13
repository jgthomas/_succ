
module Validate where


import           Control.Monad (unless, when)
import           Data.Maybe    (isNothing)

import           AST           (Tree (..))
import           Error         (CompilerError (SyntaxError), SyntaxError (..))
import           GenState      (GenState, throwError)
import qualified SymTab


-- | TODO
checkIfUsedInScope :: Tree -> GenState ()
checkIfUsedInScope node@(DeclarationNode name _ _) = do
        localDec <- SymTab.checkVariable name
        paramDec <- SymTab.parameterDeclared name
        when (localDec || paramDec) $
           throwError $ SyntaxError (DoubleDeclared node)
checkIfUsedInScope tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
validateCall :: Tree -> GenState ()
validateCall node@(FuncCallNode name _) = do
        callee <- SymTab.decSeqNumber name
        caller <- SymTab.currentSeqNumber
        unless (validSeq callee caller) $
            throwError $ SyntaxError (InvalidCall node)
validateCall tree = throwError $ SyntaxError (Unexpected tree)


validSeq :: Maybe Int -> Maybe Int -> Bool
validSeq Nothing Nothing   = False
validSeq Nothing (Just _)  = False
validSeq (Just _) Nothing  = False
validSeq (Just a) (Just b) = a <= b


-- | TODO
checkIfFuncDefined :: Tree -> GenState ()
checkIfFuncDefined node@(FunctionNode _ name _ _) = do
        defined <- SymTab.checkFuncDefined name
        when defined $
           throwError $ SyntaxError (DoubleDefined node)
checkIfFuncDefined tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
checkIfDefined :: Tree -> GenState ()
checkIfDefined node@(AssignmentNode name _ _) = do
        defined <- SymTab.checkVarDefined name
        when defined $
           throwError $ SyntaxError (DoubleDefined node)
checkIfDefined tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
checkIfVariable :: Tree -> GenState ()
checkIfVariable node@(FunctionNode _ name _ _) = do
        label <- SymTab.globalLabel name
        unless (isNothing label) $
            throwError $ SyntaxError (DoubleDefined node)
checkIfVariable tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
checkCountsMatch :: Int -> Tree -> GenState ()
checkCountsMatch count node@(FunctionNode _ _ paramList _) =
        when (count /= length paramList) $
           throwError $ SyntaxError (MisMatch count node)
checkCountsMatch count node@(FuncCallNode _ argList) =
        when (count /= length argList) $
           throwError $ SyntaxError (MisMatch count node)
checkCountsMatch _ tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
checkArguments :: Maybe Int -> Tree -> GenState ()
checkArguments (Just n) node@(FuncCallNode _ _) = checkCountsMatch n node
checkArguments (Just _) tree = throwError $ SyntaxError (Unexpected tree)
checkArguments Nothing tree  = throwError $ SyntaxError (Undeclared tree)


-- | TODO
checkIfFunction :: Tree -> GenState ()
checkIfFunction node@(DeclarationNode name _ _) = do
        paramNum <- SymTab.decParamCount name
        unless (isNothing paramNum) $
            throwError $ SyntaxError (DoubleDeclared node)
checkIfFunction tree = throwError $ SyntaxError (Unexpected tree)


-- | TODO
mkGlobLabel :: String -> Int -> String
mkGlobLabel name labnum = "_" ++ name ++ show labnum


-- | TODO
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- SymTab.variableOffset varName
        argPos  <- SymTab.parameterPosition varName
        globLab <- SymTab.globalLabel varName
        pure (offset, argPos, globLab)


-- | TODO
variableExists :: Tree -> GenState ()
variableExists node@(VarNode a)                   = varExists node a
variableExists node@(AddressOfNode a)             = varExists node a
variableExists node@(DereferenceNode a)           = varExists node a
variableExists node@(PointerNode a _ _)           = varExists node a
variableExists node@(AssignmentNode a _ _)        = varExists node a
variableExists node@(AssignDereferenceNode a _ _) = varExists node a
variableExists tree = throwError $ SyntaxError (Unexpected tree)


varExists :: Tree -> String -> GenState ()
varExists node varName = do
        (offset, argPos, globLab) <- getVariables varName
        when (isNothing offset && isNothing argPos && isNothing globLab) $
            throwError $ SyntaxError (Unrecognised node)


