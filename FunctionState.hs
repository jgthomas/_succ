
module FunctionState (FuncState(..), FuncStates(..)) where


import qualified Data.Map as M


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
                    , argCount   :: Int
                    , parameters :: FuncParams }
               deriving Show
