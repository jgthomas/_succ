{-|
Module       : SymTab
Description  : GenState control abstraction

Central interface for all functions used to manipulate the
generator stage state.
-}
module SymTab
        (module FrameStack,
         module Global,
         module Local,
         module GenState,
         getVariables
        ) where


import FrameStack (getScope)
import GenState   (GenState, labelNum)
import Global     hiding (declaredFuncType, globalType)
import Local      hiding (allTypes, parameterType, variableType)


-- | Check all variable types for the supplied identifier
getVariables :: String -> GenState (Maybe Int, Maybe Int, Maybe String)
getVariables varName = do
        offset  <- Local.variableOffset varName
        argPos  <- Local.parameterPosition varName
        globLab <- Global.globalLabel varName
        pure (offset, argPos, globLab)
