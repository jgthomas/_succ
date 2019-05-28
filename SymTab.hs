
module SymTab (newSymTab,
               addVariable,
               checkVariable,
               labelNum,
               initScope,
               variableOffset,
               closeScope,
               stackPointerValue,
               setBreak,
               getBreak,
               setContinue,
               getContinue,
               initFunction,
               closeFunction,
               addParameter,
               parameterPosition,
               nextArgumentPos,
               resetArguments,
               addDeclaration,
               decParamCount,
               decSeqNumber,
               currentSeqNumber,
               functionDefined,
               parameterDeclared) where


import qualified Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (SymTab(Tab, labelNo, offset))
import Declarations (newDecTable,
                     addDeclaration,
                     decParamCount,
                     decSeqNumber,
                     currentSeqNumber)
import SimpleStack (newStack,
                    currentFunction,
                    popFunctionName,
                    pushFunctionName)
import FunctionState (newFuncState,
                      addParameter,
                      parameterPosition,
                      parameterDeclared,
                      nextArgumentPos,
                      resetArguments)
import Scope (initScope,
              closeScope,
              functionDefined,
              getBreak,
              setBreak,
              getContinue,
              setContinue,
              checkVariable,
              variableOffset,
              newScopeRecord,
              newFuncScopesData,
              storeVar)


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


initFunction :: String -> Evaluator ()
initFunction name = do
        pushFunctionName name
        newScopeRecord name
        newFuncScopesData name
        newFuncState name
        return ()


closeFunction :: Evaluator Bool
closeFunction = do
        popFunctionName


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
        let num = labelNo symTab
            symTab' = symTab { labelNo = succ num }
            in
        (num, symTab')


notFound :: Int
notFound = -1

memOffsetSize :: Int
memOffsetSize = (-8)

firstLabel :: Int
firstLabel = 1

baseScope :: Int
baseScope = 0
