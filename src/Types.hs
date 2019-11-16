
module Types where


import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.Except (ExceptT)

import Error (CompilerError)


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)


newtype Stack a = Stack [a] deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , funcDecSeq   :: M.Map String Int
                          , funcParams   :: M.Map String Int
                          , funcTypes    :: M.Map String Type
                          , declaredVars :: M.Map String GlobalVar
                          , definedVars  :: S.Set String
                          , definedFuncs :: S.Set String
                          , varsToinit   :: [String] }
                 deriving (Show)


data FuncState = Fs { paramCount   :: Int
                    , funcOffset   :: Int
                    , currentScope :: Int
                    , parameters   :: M.Map String ParamVar
                    , scopes       :: M.Map Int (M.Map String LocalVar) }
               deriving (Show)


data Type = IntVar
          | IntPointer
          | Label
          deriving (Show, Eq)


data LocalVar = LocVar { locOffset :: Int
                       , locType   :: Type }
              deriving (Show)


data GlobalVar = GloVar { globLabel :: String
                        , globType  :: Type }
               deriving (Show)


data ParamVar = ParVar { paramNum  :: Int
                       , paramType :: Type }
              deriving (Show)


type CompilerM m = ExceptT CompilerError m
