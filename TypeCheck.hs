
module TypeCheck (paramsMatchparams,
                  argsMatchParams) where

import Evaluator (Evaluator)
import AST       (Tree(..))
import Types     (Type(..))
import Tokens    (Operator(..))
import SymTab    (allTypes,
                  variableType,
                  parameterType,
                  globalType)


paramsMatchparams :: String -> [Tree] -> Evaluator ()
paramsMatchparams name treeList = do
        (oldParams, newParams) <- passedTypes name treeList
        let errorType = (ParamParam name oldParams newParams)
        checkPassedTypes oldParams newParams errorType


argsMatchParams :: String -> [Tree] -> Evaluator ()
argsMatchParams name treeList = do
        (params, args) <- passedTypes name treeList
        let errorType = (ArgParam name params args)
        checkPassedTypes params args errorType


passedTypes :: String -> [Tree] -> Evaluator ([Type], [Type])
passedTypes name treeList = do
        currTypes <- SymTab.allTypes name
        newTypes  <- mapM getType treeList
        return (currTypes, newTypes)


checkPassedTypes :: [Type] -> [Type] -> TypeError -> Evaluator ()
checkPassedTypes oldTypes newTypes errorType =
        if oldTypes == newTypes
           then return ()
           else error $ typeError errorType


getType :: Tree -> Evaluator Type
getType (ArgNode tree)       = getType tree
getType (ParamNode typ tree) = return typ
getType (VarNode name)       = getVariableType name
getType (AddressOfNode name) = return IntPointer
getType (TernaryNode l m r)  = getTernaryType l m r
getType (BinaryNode l r op)  = getBinaryType l r op
getType (UnaryNode tree op)  = getType tree
getType (ConstantNode const) = return IntVar


getVariableType :: String -> Evaluator Type
getVariableType name = do
        typL <- SymTab.variableType name
        typP <- SymTab.parameterType name
        typG <- SymTab.globalType name
        case varType typL typP typG of
             Just t  -> return t
             Nothing -> error $ typeError (NoType name)


varType :: Maybe Type -> Maybe Type -> Maybe Type -> Maybe Type
varType (Just typL) _ _         = (Just typL)
varType _ (Just typP) _         = (Just typP)
varType _ _ (Just typG)         = (Just typG)
varType Nothing Nothing Nothing = Nothing


getBinaryType :: Tree -> Tree -> Operator -> Evaluator Type
getBinaryType left right op = do
        leftType  <- getType left
        rightType <- getType right
        return $ binType leftType rightType op


binType :: Type -> Type -> Operator -> Type
binType IntVar IntVar _ = IntVar


getTernaryType :: Tree -> Tree -> Tree -> Evaluator Type
getTernaryType left mid right = do
        leftType  <- getType left
        midType   <- getType mid
        rightType <- getType right
        return $ ternaryType leftType midType rightType


ternaryType :: Type -> Type -> Type -> Type
ternaryType IntVar IntVar IntVar = IntVar


data TypeError = NoType String
               | ParamParam String [Type] [Type]
               | ArgParam String [Type] [Type]
               deriving Eq


typeError :: TypeError -> String

typeError (NoType name) = "no type associated with variable: " ++ name

typeError (ParamParam name oldParams newParams) =
        "declarations have mismatching parameter types: "
        ++ show oldParams ++ " vs. " ++ show newParams
        ++ " for function: " ++ name

typeError (ArgParam name params args) =
        "mismatching parameters: " ++ show params
        ++ " and arguments: " ++ show args
        ++ " for function: " ++ name
