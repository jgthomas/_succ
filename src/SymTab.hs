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
         getVariables
        ) where


import FrameStack  (currentFunc, getScope)
import FuncState
import GenState    (GenState, labelNum)
import GlobalScope
import SymbolTable (SymTab, memOffset)


-- | Check all variable types for the supplied identifier
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- FuncState.variableOffset varName
        argPos  <- FuncState.parameterPosition varName
        globLab <- GlobalScope.globalLabel varName
        pure (offset, argPos, globLab)
