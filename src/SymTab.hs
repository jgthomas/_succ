{-|
Module       : SymTab
Description  : GenState control abstraction

Central interface for all functions used to manipulate the
generator stage state.
-}
module SymTab
        (module FrameStack,
         module GlobalScope,
         module FuncState,
         SymTab,
         labelNum,
         memOffset,
         getVariables,
         getVariable
        ) where


import           FrameStack  (currentFunc, getScope)
import           FuncState
import           GenState    (GenState, labelNum)
import           GenTokens   (VarLookup (..), VarType (..))
import qualified GenTokens   (mkVarGlobal, mkVarLocal, mkVarParam)
import           GlobalScope
import           SymbolTable (SymTab, memOffset)


-- | Check all variable types for the supplied identifier
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- FuncState.variableOffset varName
        argPos  <- FuncState.parameterPosition varName
        globLab <- GlobalScope.globalLabel varName
        pure (offset, argPos, globLab)


getVariable :: String -> GenState VarLookup
getVariable name = do
        localVar  <- GenTokens.mkVarLocal <$> FuncState.variableOffset name
        paramVar  <- GenTokens.mkVarParam <$> FuncState.parameterPosition name
        globalVar <- GenTokens.mkVarGlobal <$> GlobalScope.globalLabel name
        case (localVar, paramVar, globalVar) of
             (var@(VarType LocalVar{}), _, _)  -> pure var
             (_, var@(VarType ParamVar{}), _)  -> pure var
             (_, _, var@(VarType GlobalVar{})) -> pure var
             (_, _, _)                         -> pure NotFound
