
module TypeCheck (getType) where

import Evaluator (Evaluator)
import AST       (Tree(..))
import Types     (Type(..))
import SymTab    (variableType,
                  parameterType,
                  globalType)


getType :: Tree -> Evaluator Type
getType (ArgNode tree)       = getType tree
getType (ParamNode typ tree) = return typ
getType (VarNode name)       = getVariableType name
getType (AddressOfNode name) = return IntPointer
getType _                    = return IntVar


getVariableType :: String -> Evaluator Type
getVariableType name = do
        typL <- SymTab.variableType name
        typP <- SymTab.parameterType name
        typG <- SymTab.globalType name
        case varType typL typP typG of
             Just t  -> return t
             Nothing -> error $ "no type associated with variable: " ++ name


varType :: Maybe Type -> Maybe Type -> Maybe Type -> Maybe Type
varType (Just typL) _ _         = (Just typL)
varType _ (Just typP) _         = (Just typP)
varType _ _ (Just typG)         = (Just typG)
varType Nothing Nothing Nothing = Nothing
