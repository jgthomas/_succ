
module TypeCheck (paramDeclaration,
                  funcTypeDeclaration,
                  argsMatchParams,
                  globalDeclaration,
                  assignment,
                  funcReturn) where

import Evaluator   (Evaluator)
import AST         (Tree(..))
import Types       (Type(..))
import Tokens      (Operator(..))
import FrameStack  (currentScope)
import GlobalScope (globalType, declaredFuncType)
import FuncState   (allTypes, variableType, parameterType)


paramDeclaration :: String -> [Tree] -> Evaluator ()
paramDeclaration name treeList = do
        (oldParams, newParams) <- passedTypes name treeList
        let errorType = ParamParam name oldParams newParams
        checkTypes oldParams newParams errorType


argsMatchParams :: String -> [Tree] -> Evaluator ()
argsMatchParams name treeList = do
        (params, args) <- passedTypes name treeList
        let errorType = ArgParam name params args
        checkTypes params args errorType


funcTypeDeclaration :: String -> Type -> Evaluator ()
funcTypeDeclaration name newTyp = do
        oldTyp <- getFuncType name
        let errorType = FuncType name oldTyp newTyp
        checkTypes [oldTyp] [newTyp] errorType


globalDeclaration :: String -> Type -> Evaluator ()
globalDeclaration name newTyp = do
        oldTyp <- getType (VarNode name)
        let errorType = VarType name oldTyp newTyp
        checkTypes [oldTyp] [newTyp] errorType


assignment :: String -> Tree -> Evaluator ()
assignment name value = do
        varTyp  <- getType (VarNode name)
        valType <- getType value
        if valType `elem` permitted varTyp
           then return ()
           else error $ typeError (Assignment name varTyp valType)


funcReturn :: Tree -> Evaluator ()
funcReturn retVal = do
        currFuncName <- currentScope
        currFuncType <- getFuncType currFuncName
        retValType   <- getType retVal
        let errorType = FuncReturn currFuncName currFuncType retValType
        checkTypes [currFuncType] [retValType] errorType


-- Internal

passedTypes :: String -> [Tree] -> Evaluator ([Type], [Type])
passedTypes name treeList = do
        currTypes <- allTypes name
        newTypes  <- mapM getType treeList
        return (currTypes, newTypes)


checkTypes :: [Type] -> [Type] -> TypeError -> Evaluator ()
checkTypes oldTypes newTypes errorType =
        if oldTypes == newTypes
           then return ()
           else error $ typeError errorType


getType :: Tree -> Evaluator Type
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


getVariableType :: String -> Evaluator Type
getVariableType name = do
        currScope <- currentScope
        if currScope == "global"
           then do
                   typG <- globalType name
                   extractType name typG
           else checkAllVariableTypes name


checkAllVariableTypes :: String -> Evaluator Type
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


extractType :: String -> Maybe Type -> Evaluator Type
extractType _ (Just typ) = return typ
extractType name Nothing = error $ typeError (NoType name)


getBinaryType :: Tree -> Tree -> Operator -> Evaluator Type
getBinaryType left right op = do
        leftType  <- getType left
        rightType <- getType right
        return $ binType leftType rightType op


binType :: Type -> Type -> Operator -> Type
binType IntVar IntVar _ = IntVar
binType _ _ _ = undefined


getTernaryType :: Tree -> Tree -> Tree -> Evaluator Type
getTernaryType left mid right = do
        leftType  <- getType left
        midType   <- getType mid
        rightType <- getType right
        return $ ternaryType leftType midType rightType


ternaryType :: Type -> Type -> Type -> Type
ternaryType IntVar IntVar IntVar = IntVar
ternaryType _ _ _ = undefined


permitted :: Type -> [Type]
permitted IntVar     = [IntVar,IntPointer]
permitted IntPointer = [IntVar,IntPointer]
permitted typ        = error $ "unrecognised type: " ++ show typ


getFuncType :: String -> Evaluator Type
getFuncType name = do
        oldTyp <- declaredFuncType name
        extractType name oldTyp


addressOfType :: String -> Evaluator Type
addressOfType name = do
        typ <- getType (VarNode name)
        addType typ


addType :: Type -> Evaluator Type
addType IntVar = return IntPointer
addType _ = undefined


dereferenceType :: String -> Evaluator Type
dereferenceType name = do
        typ <- getType (VarNode name)
        derefType typ


derefType :: Type -> Evaluator Type
derefType IntPointer = return IntVar
derefType _ = undefined


data TypeError = NoType String
               | ParamParam String [Type] [Type]
               | ArgParam String [Type] [Type]
               | VarType String Type Type
               | Assignment String Type Type
               | FuncType String Type Type
               | FuncReturn String Type Type
               deriving Eq


typeError :: TypeError -> String

typeError (NoType name) = "no type associated with: " ++ name

typeError (ParamParam name oldParams newParams) =
        "declarations have mismatching parameter types: "
        ++ show oldParams ++ " vs. " ++ show newParams
        ++ " for function: " ++ name

typeError (ArgParam name params args) =
        "mismatching parameters: " ++ show params
        ++ " and arguments: " ++ show args
        ++ " for function: " ++ name

typeError (VarType name oldTyp newTyp) =
        "previous declaration of " ++ name
        ++ " as: " ++ show oldTyp
        ++ " new declaration as: " ++ show newTyp

typeError (Assignment name varTyp valTyp) =
        "cannot assign: " ++ show valTyp
        ++ " to variable: " ++ name
        ++ " of type: " ++ show varTyp

typeError (FuncType name oldTyp newTyp) =
        "previous declaration of " ++ name
        ++ " has return value: " ++ show oldTyp
        ++ " new declaration has return value: " ++ show newTyp

typeError (FuncReturn name decTyp retTyp) =
        "attempting to return: " ++ show retTyp
        ++ " from function " ++ name
        ++ " which has type: " ++ show decTyp
