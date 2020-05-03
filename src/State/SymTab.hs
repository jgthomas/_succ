{-|
Module       : SymTab
Description  : GenState control abstraction

Central interface for all functions used to manipulate the
generator stage state.
-}
module State.SymTab
        (module State.FrameStack,
         module State.GlobalScope,
         module State.FuncState,
         SymTab,
         labelNum,
         memOffset,
         getVariable
        ) where


import           GenState          (GenState, labelNum)
import           GenTokens         (VarLookup (..), VarType (..))
import qualified GenTokens         (mkVarGlobal, mkVarLocal, mkVarParam)
import           State.FrameStack  (currentFunc, getScope)
import           State.FuncState
import           State.GlobalScope
import           State.SymbolTable (SymTab, memOffset)


-- | Build variable data type from retrieved data
getVariable :: String -> GenState VarLookup
getVariable name = do
        localVar  <- GenTokens.mkVarLocal <$> State.FuncState.variableOffset name
        paramVar  <- GenTokens.mkVarParam <$> State.FuncState.parameterPosition name
        globalVar <- GenTokens.mkVarGlobal <$> State.GlobalScope.globalLabel name
        case (localVar, paramVar, globalVar) of
             (var@(VarType LocalVar{}), _, _)  -> pure var
             (_, var@(VarType ParamVar{}), _)  -> pure var
             (_, _, var@(VarType GlobalVar{})) -> pure var
             (_, _, _)                         -> pure NotFound
