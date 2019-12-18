{-|
Module       : GlobalScope
Description  : Global scope state

-}
module GlobalScope where


import qualified Data.Map as M
import qualified Data.Set as S

import           Type     (Type)


-- | Global scope state definition
data GlobalScope = Gscope { seqNum       :: Int
                          , funcDecSeq   :: M.Map String Int
                          , funcParams   :: M.Map String Int
                          , funcTypes    :: M.Map String Type
                          , declaredVars :: M.Map String GlobalVar
                          , definedVars  :: S.Set String
                          , definedFuncs :: S.Set String
                          , varsToInit   :: [String] }
                 deriving (Show)


-- | Global scope state constructor
mkGlobalScope :: GlobalScope
mkGlobalScope = Gscope 0 M.empty M.empty M.empty M.empty S.empty S.empty []


-- | Global variable state definition
data GlobalVar = GloVar { globLabel :: String
                        , globType  :: Type }
               deriving (Show)


-- | Global variable state constructor
mkGloVar :: String -> Type -> GlobalVar
mkGloVar l t = GloVar l t
