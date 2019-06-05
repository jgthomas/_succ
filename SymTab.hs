
module SymTab (currentScope,
               labelNum,
               newSymTab,
               module GlobalScope,
               module FuncState) where


import qualified Data.Map as M

import GlobalScope
import FuncState
import Evaluator    (Evaluator(Ev))
import Types        (SymTab(Tab, label))
import FrameStack   (newStack, currentScope)


{- API -}

newSymTab :: SymTab
newSymTab = Tab
            firstLabel
            memOffsetSize
            newStack
            newGlobalScope
            M.empty


labelNum :: Evaluator Int
labelNum = do
        nextLabel


nextLabel :: Evaluator Int
nextLabel = Ev $ \symTab ->
        let num = label symTab
            symTab' = symTab { label = succ num }
            in
        (num, symTab')


firstLabel :: Int
firstLabel = 1
