
module Types where


import qualified Data.Map as M
import qualified Data.Set as S


data SymTab = Tab { label       :: Int
                  , frameStack  :: Stack String
                  , globalScope :: GlobalScope
                  , funcStates  :: M.Map String FuncState }
            deriving (Show)


newtype Stack a = Stack [a] deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , funcDecSeq   :: M.Map String Int
                          , funcParams   :: M.Map String Int
                          , declaredVars :: M.Map String String
                          , definedVars  :: S.Set String }
                 deriving (Show)


data FuncState = Fs { paramCount   :: Int
                    , funcOffset   :: Int
                    , currentScope :: Int
                    , parameters   :: M.Map String Int
                    , scopes       :: M.Map Int (M.Map String Int) }
               deriving (Show)
