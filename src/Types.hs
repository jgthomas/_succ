
module Types where


import qualified Data.Map    as M

import           GlobalScope (GlobalScope, mkGlobalScope)
import           Stack       (Stack, mkStack)
import           VarTypes    (Type)


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)

mkSymTab :: SymTab
mkSymTab = Tab 1 mkStack mkGlobalScope M.empty


data FuncState = Fs
               { paramCount   :: Int
               , funcOffset   :: Int
               , currentScope :: Int
               , parameters   :: M.Map String ParamVar
               , scopes       :: M.Map Int (M.Map String LocalVar) }
               deriving (Show)

mkFS :: FuncState
mkFS = Fs 0 (-8) 0 M.empty (M.singleton 0 M.empty)


data LocalVar = LocVar { locOffset :: Int
                       , locType   :: Type }
              deriving (Show)

mkLocVar :: Int -> Type -> LocalVar
mkLocVar n t = LocVar n t


data ParamVar = ParVar { paramNum  :: Int
                       , paramType :: Type }
              deriving (Show)

mkParVar :: Int -> Type -> ParamVar
mkParVar n t = ParVar n t
