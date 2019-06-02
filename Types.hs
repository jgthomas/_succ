
module Types where


import qualified Data.Map as M


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope

{-
- State of the whole program
-
- labelNo      : the number of the next label to output in asm
- offset       : the offset from %rbp where the next local variable should be stored
- nameStack    : stack of program function calls, current function at top
- declarations : key=function name, value=number indicating sequence of when declared
- funcStates   : key=function name, value=state container for that function
- scopeLevels  : key=function name, value=current scope depth in that function
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
                  , funcStates   :: FuncStates
                  , scopeLevels  :: M.Map String Int
                  , scopesData   :: ProgramScope }
            deriving Show


--type SeqNums = M.Map String Int
--type ParamCounts = M.Map String Int
--
--data Declared = D { seqNum    :: Int
--                  , declOrder :: SeqNums
--                  , parameter :: ParamCounts }
--              deriving Show


data GlobalScope = Gscope { seqNum       :: Int
                          , declarations :: M.Map String Int
                          , decParams    :: M.Map String Int
                          , globalVars   :: M.Map String String }
                 deriving (Show)


newtype Stack a = Stack [a] deriving Show


type FuncParams = M.Map String Int
type FuncStates = M.Map String FuncState

{-
- State of a function
-
- paramCount : the number of parameters the function has
- argCount   : counter for arguments passed
- parameters : key=parameter name, value=parameter position
-
-}
data FuncState = Fs { paramCount :: Int
                    , parameters :: FuncParams }
               deriving Show
