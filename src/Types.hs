
module Types where


import qualified Data.Map as M
import qualified Data.Set as S


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)


mkSymTab :: SymTab
mkSymTab = Tab 0 mkStack mkGS M.empty


newtype Stack a = Stack [a] deriving Show


mkStack :: Stack a
mkStack = Stack []


data GlobalScope = Gscope { seqNum       :: Int
                          , funcDecSeq   :: M.Map String Int
                          , funcParams   :: M.Map String Int
                          , funcTypes    :: M.Map String Type
                          , declaredVars :: M.Map String GlobalVar
                          , definedVars  :: S.Set String
                          , definedFuncs :: S.Set String
                          , varsToinit   :: [String] }
                 deriving (Show)


mkGS :: GlobalScope
mkGS = Gscope 0 M.empty M.empty M.empty M.empty S.empty S.empty []


data FuncState = Fs { paramCount   :: Int
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


data GlobalVar = GloVar { globLabel :: String
                        , globType  :: Type }
               deriving (Show)


mkGloVar :: String -> Type -> GlobalVar
mkGloVar l t = GloVar l t


data ParamVar = ParVar { paramNum  :: Int
                       , paramType :: Type }
              deriving (Show)


mkParVar :: Int -> Type -> ParamVar
mkParVar n t = ParVar n t


data Type = IntVar
          | IntPointer
          | Label
          deriving (Show, Eq)
