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
import Tokens      (Operator(..))
import SuccState   (throwError)
import FrameStack  (currentScope)
import GlobalScope (globalType, declaredFuncType)
import FuncState   (allTypes, variableType, parameterType)
import GenState    (GenState)


-- | Throws error if two lists of types don't match
typesMatch :: String -> [Tree] -> GenState ()
typesMatch name treeList = do
        (params, args) <- passedTypes name treeList
        checkTypes params args


funcDeclaration :: String -> Type -> GenState ()
funcDeclaration name newTyp = do
        oldTyp <- getFuncType name
        checkTypes [oldTyp] [newTyp]


globalDeclaration :: String -> Type -> GenState ()
globalDeclaration name newTyp = do
        oldTyp <- getType (VarNode name)
        checkTypes [oldTyp] [newTyp]


assignment :: String -> Tree -> GenState ()
assignment name value = do
        varTyp  <- getType (VarNode name)
        valType <- getType value
        valid   <- permitted varTyp
        unless (valType `elem` valid) $
            throwError $ TypeError (TypeMismatch [varTyp] [valType])


funcReturn :: Tree -> GenState ()
funcReturn retVal = do
        currFuncName <- currentScope
        currFuncType <- getFuncType currFuncName
        retValType   <- getType retVal
        checkTypes [currFuncType] [retValType]


-- Internal

passedTypes :: String -> [Tree] -> GenState ([Type], [Type])
passedTypes name treeList = do
        currTypes <- allTypes name
        newTypes  <- mapM getType treeList
        return (currTypes, newTypes)


checkTypes :: [Type] -> [Type] -> GenState ()
checkTypes oldTypes newTypes =
        when (oldTypes /= newTypes) $
            throwError $ TypeError (TypeMismatch oldTypes newTypes)


getType :: Tree -> GenState Type
getType (ArgNode tree)            = getType tree
getType (ParamNode typ _)         = return typ
getType (VarNode name)            = getVariableType name
getType (AddressOfNode name)      = addressOfType name
getType (TernaryNode l m r)       = getTernaryType l m r
getType (BinaryNode l r op)       = getBinaryType l r op
getType (UnaryNode tree _)        = getType tree
getType (ConstantNode _)          = return IntVar
getType (FuncCallNode name _)     = getFuncType name
getType (AssignmentNode _ tree _) = getType tree
getType (DereferenceNode name)    = dereferenceType name
getType _                         = undefined


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
        extractType name $ varType typL typP typG


varType :: Maybe Type -> Maybe Type -> Maybe Type -> Maybe Type
varType (Just typL) _ _         = Just typL
varType _ (Just typP) _         = Just typP
varType _ _ (Just typG)         = Just typG
varType Nothing Nothing Nothing = Nothing


extractType :: String -> Maybe Type -> GenState Type
extractType _ (Just typ) = return typ
extractType name Nothing = throwError $ TypeError (MissingType name)


getBinaryType :: Tree -> Tree -> Operator -> GenState Type
getBinaryType left right op = do
        leftType  <- getType left
        rightType <- getType right
        return $ binType leftType rightType op


binType :: Type -> Type -> Operator -> Type
binType IntVar IntVar _ = IntVar
binType _ _ _ = undefined


getTernaryType :: Tree -> Tree -> Tree -> GenState Type
getTernaryType left mid right = do
        leftType  <- getType left
        midType   <- getType mid
        rightType <- getType right
        return $ ternaryType leftType midType rightType


ternaryType :: Type -> Type -> Type -> Type
ternaryType IntVar IntVar IntVar = IntVar
ternaryType _ _ _ = undefined


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
addType IntVar = return IntPointer
addType _ = undefined


dereferenceType :: String -> GenState Type
dereferenceType name = do
        typ <- getType (VarNode name)
        derefType typ


derefType :: Type -> GenState Type
derefType IntPointer = pure IntVar
derefType typ = throwError $ TypeError (UnexpectedType typ)
