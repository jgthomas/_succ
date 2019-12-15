
module LocalScope where


import qualified Data.Map as M

import           Type     (Type)


data FuncState = Fs
               { paramCount   :: Int
               , funcOffset   :: Int
               , currentScope :: Int
               , parameters   :: M.Map String ParamVar
               , scopes       :: M.Map Int (M.Map String LocalVar) }
               deriving (Show)


mkFuncState :: FuncState
mkFuncState = Fs 0 (-8) 0 M.empty (M.singleton 0 M.empty)


data LocalVar = LocVar
              { locOffset :: Int
              , locType   :: Type }
              deriving (Show)


mkLocVar :: Int -> Type -> LocalVar
mkLocVar n t = LocVar n t


data ParamVar = ParVar
              { paramNum  :: Int
              , paramType :: Type }
              deriving (Show)


mkParVar :: Int -> Type -> ParamVar
mkParVar n t = ParVar n t
