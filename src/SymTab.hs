{-|
Module       : SymTab
Description  : GenState control abstraction

Central interface for all functions used to manipulate the
generator stage state.
-}
module SymTab
        (module FrameStack,
         module GlobalScope,
         module SymTabLocal,
         labelNum,
         memOffset,
         getVariables
        ) where


import FrameStack  (currentFunc, getScope)
import GenState    (GenState, labelNum)
import GlobalScope
import SymbolTable (memOffset)
import SymTabLocal


-- | Check all variable types for the supplied identifier
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- SymTabLocal.variableOffset varName
        argPos  <- SymTabLocal.parameterPosition varName
        globLab <- GlobalScope.globalLabel varName
        pure (offset, argPos, globLab)
