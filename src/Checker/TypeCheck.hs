{-|
Module       : TypeCheck
Description  : Checks for type errors

Provides basic type checking capabilities.
-}
module Checker.TypeCheck
        (typesMatch,
         funcDeclaration,
         globalDeclaration,
         assignment,
         funcReturn
        ) where

import           Control.Monad   (unless, when)

import           Error.Error     (CheckerError (..), CompilerError (CheckerError, ImpossibleError, TypeError),
                                  TypeError (..))
import           GenState        (GenState, throwError)
import qualified State.SymTab    as SymTab
import           Types.AST       (ArrayNode (..), Tree (..))
import           Types.Type      (Type (..))
import           Types.Variables (Scope (..))


-- | Throw error if two lists of types don't match
typesMatch :: Tree -> GenState ()
typesMatch node@(FuncCallNode name argList _) =
        checkTypesMatch node name argList
typesMatch node@(FunctionNode _ name paramList _ _) =
        checkTypesMatch node name paramList
typesMatch tree = throwError $ CheckerError (InvalidNode tree)


checkTypesMatch :: Tree -> String -> [Tree] -> GenState ()
checkTypesMatch node name treeList = do
        (params, args) <- passedTypes name treeList
        checkTypes node params args


-- | Throw error if function type declarations don't match
funcDeclaration :: Tree -> GenState ()
funcDeclaration node@(FunctionNode typ name _ _ _) = do
        oldTyp <- getFuncType node name
        checkTypes node [oldTyp] [typ]
funcDeclaration tree = throwError $ CheckerError (InvalidNode tree)


-- | Throw error if global variable type declarations don't match
globalDeclaration :: Tree -> GenState ()
globalDeclaration node@(DeclarationNode varNode typ _ _) = do
        oldTyp <- getType varNode
        checkTypes node [oldTyp] [typ]
globalDeclaration tree = throwError $ CheckerError (InvalidNode tree)


-- | Throw error if types of assignment and declaration don't match
assignment :: Tree -> GenState ()
assignment (AssignmentNode varNode value _ _) =
        checkAssignmentType varNode value
assignment (AssignDereferenceNode varNode value _ _) =
        checkAssignmentType varNode value
assignment node = throwError $ CheckerError (InvalidNode node)


checkAssignmentType :: Tree -> Tree -> GenState ()
checkAssignmentType varNode value = do
        varTyp  <- getType varNode
        valType <- getType value
        valid   <- permitted varTyp
        unless (valType `elem` valid) $
            throwError $ TypeError (TypeMismatch [varTyp] [valType] varNode)


-- | Throw error if declared and actual return value don't match
funcReturn :: Tree -> Tree -> GenState ()
funcReturn node retVal = do
        currFuncName <- SymTab.currentFunc
        currFuncType <- getFuncType node currFuncName
        retValType   <- getType retVal
        checkTypes node [currFuncType] [retValType]


passedTypes :: String -> [Tree] -> GenState ([Type], [Type])
passedTypes name treeList = do
        currTypes <- SymTab.allTypes name
        newTypes  <- mapM getType treeList
        pure (currTypes, newTypes)


checkTypes :: Tree -> [Type] -> [Type] -> GenState ()
checkTypes node oldTypes newTypes =
        when (oldTypes /= newTypes) $
            throwError $ TypeError (TypeMismatch oldTypes newTypes node)


getType :: Tree -> GenState Type
getType (ArgNode tree _)              = getType tree
getType (ParamNode typ _ _)           = pure typ
getType node@(VarNode name _)         = getVarType node name
getType node@(AddressOfNode name _)   = getVarType node name >>= invertType node
getType node@(DereferenceNode name _) = getVarType node name >>= invertType node
getType node@(TernaryNode l m r _)    = getTernaryType node l m r
getType node@(BinaryNode l r _ _)     = getBinaryType node l r
getType (UnaryNode tree _ _)          = getType tree
getType (ConstantNode _ _)            = pure IntVar
getType node@(FuncCallNode name _ _)  = getFuncType node name
getType (AssignmentNode _ tree _ _)   = getType tree
getType (ArrayNode arr)               = getArrayType arr
getType tree                          = throwError $ TypeError (NotTyped tree)


getArrayType :: ArrayNode -> GenState Type
getArrayType (ArrayItemAccess _ var _) = getType var >>= getArrayItemType
getArrayType (ArrayItemAssign _ var _) = getType var >>= getArrayItemType
getArrayType arrayNode = throwError $ TypeError (NotTyped (ArrayNode arrayNode))


getArrayItemType :: Type -> GenState Type
getArrayItemType IntArray = pure IntVar
getArrayItemType _        = undefined


getVarType :: Tree -> String -> GenState Type
getVarType node name = do
        currScope <- SymTab.getScope
        case currScope of
             Local  -> checkAllVariableTypes node name
             Global -> do
                     typG <- SymTab.globalType name
                     extractType node typG


checkAllVariableTypes :: Tree -> String -> GenState Type
checkAllVariableTypes node name = do
        typL <- SymTab.variableType name
        typP <- SymTab.parameterType name
        typG <- SymTab.globalType name
        typ  <- varType typL typP typG
        extractType node typ


varType :: Maybe Type -> Maybe Type -> Maybe Type -> GenState (Maybe Type)
varType (Just typL) _ _         = pure $ Just typL
varType _ (Just typP) _         = pure $ Just typP
varType _ _ (Just typG)         = pure $ Just typG
varType Nothing Nothing Nothing = pure Nothing


extractType :: Tree -> Maybe Type -> GenState Type
extractType _ (Just typ) = pure typ
extractType node Nothing = throwError $ TypeError (MissingType node)


getBinaryType :: Tree -> Tree -> Tree -> GenState Type
getBinaryType node left right = do
        leftType  <- getType left
        rightType <- getType right
        binType node leftType rightType


binType :: Tree -> Type -> Type -> GenState Type
binType _ IntVar IntVar = pure IntVar
binType node t IntVar   = throwError $ TypeError (UnexpectedType t node)
binType node IntVar t   = throwError $ TypeError (UnexpectedType t node)
binType node t _        = throwError $ TypeError (UnexpectedType t node)


getTernaryType ::  Tree -> Tree -> Tree -> Tree -> GenState Type
getTernaryType node left mid right = do
        leftType  <- getType left
        midType   <- getType mid
        rightType <- getType right
        ternaryType node leftType midType rightType


ternaryType :: Tree -> Type -> Type -> Type -> GenState Type
ternaryType _ IntVar IntVar IntVar = pure IntVar
ternaryType node t IntVar IntVar   = throwError $ TypeError (UnexpectedType t node)
ternaryType node IntVar t IntVar   = throwError $ TypeError (UnexpectedType t node)
ternaryType node IntVar IntVar t   = throwError $ TypeError (UnexpectedType t node)
ternaryType node t _ _             = throwError $ TypeError (UnexpectedType t node)


permitted :: Type -> GenState [Type]
permitted typ =
        case typ of
             IntVar     -> pure [IntVar, IntPointer]
             IntPointer -> pure [IntVar, IntPointer]
             IntArray   -> pure [IntArray]
             Label      -> throwError ImpossibleError


getFuncType :: Tree -> String -> GenState Type
getFuncType node name = do
        oldTyp <- SymTab.declaredFuncType name
        extractType node oldTyp


invertType :: Tree -> Type -> GenState Type
invertType AddressOfNode{} IntVar       = pure IntPointer
invertType DereferenceNode{} IntPointer = pure IntVar
invertType node t = throwError $ TypeError (UnexpectedType t node)
