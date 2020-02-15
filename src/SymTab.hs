{-|
Module       : SymTab
Description  : GenState control abstraction

Central interface for all functions used to manipulate the
generator stage state.
-}
module SymTab
        (module FrameStack,
         module SymTabGlobal,
         module SymTabLocal,
         module GenState,
         getVariables
        ) where


import FrameStack   (getScope)
import GenState     (GenState, labelNum)
import SymTabGlobal hiding (declaredFuncType, globalType)
import SymTabLocal  hiding (allTypes, parameterType, variableType)


-- | Check all variable types for the supplied identifier
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- SymTabLocal.variableOffset varName
        argPos  <- SymTabLocal.parameterPosition varName
        globLab <- SymTabGlobal.globalLabel varName
        pure (offset, argPos, globLab)
