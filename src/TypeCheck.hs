{-|
Module       : TypeCheck
Description  : Checks for type errors

Provides basic type checking capabilities.
-}
module TypeCheck
        (typesMatch,
         funcDeclaration,
         globalDeclaration,
         assignment,
         funcReturn
        ) where

import Control.Monad (unless, when)

import AST         (Tree(..))
import VarTypes    (Type(..))
import Error       (CompilerError(TypeError, ImpossibleError), TypeError(..))
import SuccState   (throwError)
import FrameStack  (currentScope)
import GlobalScope (globalType, declaredFuncType)
import FuncState   (allTypes, variableType, parameterType)
import GenState    (GenState)


-- | Throw error if two lists of types don't match
typesMatch :: String -> [Tree] -> GenState ()
typesMatch name treeList = do
        (params, args) <- passedTypes name treeList
        checkTypes params args


-- | Throw error if function type declarations don't match
funcDeclaration :: String -> Type -> GenState ()
funcDeclaration name newTyp = do
        oldTyp <- getFuncType name
        checkTypes [oldTyp] [newTyp]


-- | Throw error if global variable type declarations don't match
globalDeclaration :: String -> Type -> GenState ()
globalDeclaration name newTyp = do
        oldTyp <- getType (VarNode name)
        checkTypes [oldTyp] [newTyp]


-- | Throw error if types of assignment and declaration don't match
assignment :: String -> Tree -> GenState ()
assignment name value = do
        varTyp  <- getType (VarNode name)
        valType <- getType value
        valid   <- permitted varTyp
        unless (valType `elem` valid) $
            throwError $ TypeError (TypeMismatch [varTyp] [valType])


-- | Throw error if declared and actual return value don't match
funcReturn :: Tree -> GenState ()
funcReturn retVal = do
        currFuncName <- currentScope
        currFuncType <- getFuncType currFuncName
        retValType   <- getType retVal
        checkTypes [currFuncType] [retValType]


passedTypes :: String -> [Tree] -> GenState ([Type], [Type])
passedTypes name treeList = do
        currTypes <- allTypes name
        newTypes  <- mapM getType treeList
        pure (currTypes, newTypes)


checkTypes :: [Type] -> [Type] -> GenState ()
checkTypes oldTypes newTypes =
        when (oldTypes /= newTypes) $
            throwError $ TypeError (TypeMismatch oldTypes newTypes)


getType :: Tree -> GenState Type
getType (ArgNode tree)            = getType tree
getType (ParamNode typ _)         = pure typ
getType (VarNode name)            = getVariableType name
getType (AddressOfNode name)      = addressOfType name
getType (TernaryNode l m r)       = getTernaryType l m r
getType (BinaryNode l r _)        = getBinaryType l r
getType (UnaryNode tree _)        = getType tree
getType (ConstantNode _)          = pure IntVar
getType (FuncCallNode name _)     = getFuncType name
getType (AssignmentNode _ tree _) = getType tree
getType (DereferenceNode name)    = dereferenceType name
getType tree                      = throwError $ TypeError (NotTyped tree)


getVariableType :: String -> GenState Type
getVariableType name = do
        currScope <- currentScope
        if currScope == "global"
           then do
                   typG <- globalType name
                   extractType name typG
           else checkAllVariableTypes name


checkAllVariableTypes :: String -> GenState Type
checkAllVariableTypes name = do
        typL <- variableType name
        typP <- parameterType name
        typG <- globalType name
        typ  <- varType typL typP typG
        extractType name typ


varType :: Maybe Type -> Maybe Type -> Maybe Type -> GenState (Maybe Type)
varType (Just typL) _ _         = pure $ Just typL
varType _ (Just typP) _         = pure $ Just typP
varType _ _ (Just typG)         = pure $ Just typG
varType Nothing Nothing Nothing = pure Nothing


extractType :: String -> Maybe Type -> GenState Type
extractType _ (Just typ) = pure typ
extractType name Nothing = throwError $ TypeError (MissingType name)


getBinaryType :: Tree -> Tree -> GenState Type
getBinaryType left right = do
        leftType  <- getType left
        rightType <- getType right
        binType leftType rightType


binType :: Type -> Type -> GenState Type
binType IntVar IntVar = pure IntVar
binType typ IntVar = throwError $ TypeError (UnexpectedType typ)
binType IntVar typ = throwError $ TypeError (UnexpectedType typ)
binType a _        = throwError $ TypeError (UnexpectedType a)


getTernaryType :: Tree -> Tree -> Tree -> GenState Type
getTernaryType left mid right = do
        leftType  <- getType left
        midType   <- getType mid
        rightType <- getType right
        ternaryType leftType midType rightType


ternaryType :: Type -> Type -> Type -> GenState Type
ternaryType IntVar IntVar IntVar = pure IntVar
ternaryType a IntVar IntVar = throwError $ TypeError (UnexpectedType a)
ternaryType IntVar b IntVar = throwError $ TypeError (UnexpectedType b)
ternaryType IntVar IntVar c = throwError $ TypeError (UnexpectedType c)
ternaryType a _ _           = throwError $ TypeError (UnexpectedType a)


permitted :: Type -> GenState [Type]
permitted typ =
        case typ of
             IntVar     -> pure [IntVar, IntPointer]
             IntPointer -> pure [IntVar, IntPointer]
             Label      -> throwError ImpossibleError


getFuncType :: String -> GenState Type
getFuncType name = do
        oldTyp <- declaredFuncType name
        extractType name oldTyp


addressOfType :: String -> GenState Type
addressOfType name = do
        typ <- getType (VarNode name)
        addType typ


addType :: Type -> GenState Type
addType IntVar = pure IntPointer
addType typ = throwError $ TypeError (UnexpectedType typ)


dereferenceType :: String -> GenState Type
dereferenceType name = do
        typ <- getType (VarNode name)
        derefType typ


derefType :: Type -> GenState Type
derefType IntPointer = pure IntVar
derefType typ = throwError $ TypeError (UnexpectedType typ)
