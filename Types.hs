
module Types where


import qualified Data.Map as M


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope

{-
- scopesData   : triply nested map tracking the variables in each scope
-    ProgramScope
-          key=function name, value=map of the scopes in each function
-    FunctionScope
-          key=scope level in function, value=map of variables in that scope
-    LocalScope
-          key=variable name, value=offset from %rbp where stored
-
-}
data SymTab = Tab { label        :: Int
                  , offset       :: Int
                  , frameStack   :: Stack String
                  , globalScope  :: GlobalScope
                  , funcStates   :: M.Map String FuncState
                  , scopesData   :: ProgramScope }
            deriving Show


newtype Stack a = Stack [a] deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , declarations :: M.Map String Int
                          , decParams    :: M.Map String Int
                          , globalVars   :: M.Map String String }
                 deriving (Show)


data FuncState = Fs { paramCount   :: Int
                    , currentScope :: Int
                    , parameters   :: M.Map String Int }
               deriving Show
