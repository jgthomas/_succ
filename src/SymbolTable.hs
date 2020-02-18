
module SymbolTable (SymTab(..), mkSymTab) where


import qualified Data.Map       as M

import           GenStateGlobal (GlobalScope, mkGlobalScope)
import           GenStateLocal  (FuncState)
import           Stack          (Stack, mkStack)


data SymTab = SymTab { label       :: Int
                     , frameStack  :: Stack String
                     , globalScope :: GlobalScope
                     , funcStates  :: M.Map String FuncState }
            deriving (Show)


mkSymTab :: SymTab
mkSymTab = SymTab 1 mkStack mkGlobalScope M.empty
