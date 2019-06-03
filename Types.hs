
module Types where


import qualified Data.Map as M


data SymTab = Tab { label        :: Int
                  , offset       :: Int
                  , frameStack   :: Stack String
                  , globalScope  :: GlobalScope
                  , funcStates   :: M.Map String FuncState
                  , scopesData   :: M.Map String (M.Map Int (M.Map String Int)) }
            deriving (Show)


newtype Stack a = Stack [a] deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , declarations :: M.Map String Int
                          , decParams    :: M.Map String Int
                          , globalVars   :: M.Map String String }
                 deriving (Show)


data FuncState = Fs { paramCount   :: Int
                    , currentScope :: Int
                    , parameters   :: M.Map String Int }
               deriving (Show)
