
module NewTypes where


import qualified Data.Map as M


data SymTab = SymTab { nextLabel   :: Int
                     , nextOffset  :: Int
                     , nameStack   :: Stack String
                     , globalScope :: GlobalScope
                     , funcScopes  :: FuncScopes }
            deriving (Show)


newtype Stack a = Stack [a] deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , declarations :: M.Map String Int
                          , decParams    :: M.Map String Int
                          , globalVars   :: M.Map String String }
                 deriving (Show)


newtype FuncScopes = Fscopes (M.Map String FuncScope) deriving Show


data FuncScope = Fscope { paramCount :: Int
                        , scopeLevel :: Int
                        , parameters :: M.Map String Int
                        , scopeData  :: FunctionScope }
               deriving (Show)


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
