
module SymTab (addVariable,
               currentScope,
               labelNum,
               newSymTab,
               stackPointerValue,
               module Declarations,
               module FunctionState,
               module Scope) where


import qualified Data.Map as M

import Declarations
import FunctionState
import Scope
import Evaluator     (Evaluator(Ev))
import Types         (SymTab(Tab, label, offset))
import FrameStack    (newStack, currentScope)


{- API -}

newSymTab :: SymTab
newSymTab = Tab
            firstLabel
            memOffsetSize
            newStack
            newDecTable
            M.empty
            M.empty
            M.empty


stackPointerValue :: Evaluator Int
stackPointerValue = do
        currOff <- currentOffset
        return $ negate currOff


addVariable :: String -> Evaluator Int
addVariable varName = do
        currOff <- currentOffset
        storeVar varName currOff
        incrementOffset currOff


labelNum :: Evaluator Int
labelNum = do
        nextLabel


{- Internal -}

currentOffset :: Evaluator Int
currentOffset = Ev $ \symTab ->
        let currOff = offset symTab
            in
        (currOff, symTab)


incrementOffset :: Int -> Evaluator Int
incrementOffset currOff = Ev $ \symTab ->
        let symTab' = symTab { offset = currOff + memOffsetSize }
            in
        (currOff, symTab')


nextLabel :: Evaluator Int
nextLabel = Ev $ \symTab ->
        let num = label symTab
            symTab' = symTab { label = succ num }
            in
        (num, symTab')


memOffsetSize :: Int
memOffsetSize = (-8)


firstLabel :: Int
firstLabel = 1
