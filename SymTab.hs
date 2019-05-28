
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
import Scope (getLocalScope,
              getFunctionScope,
              getProgramScope,
              storeVariable,
              updateFunctionScope,
              updateProgramScope,
              checkVar,
              getVar,
              incrementScope,
              decrementScope,
              findScope,
              newScopeRecord)


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
        newFuncState name
        progScope <- updateProgramScope name M.empty
        funcScope <- getFunctionScope name progScope
        funcScope' <- updateFunctionScope baseScope M.empty funcScope
        updateProgramScope name funcScope'
        return ()


closeFunction :: Evaluator Bool
closeFunction = do
        popFunctionName


initScope :: Evaluator ()
initScope = do
        currFuncName <- currentFunction
        newScopeLevel <- incrementScope
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        funcScope' <- updateFunctionScope newScopeLevel M.empty funcScope
        updateProgramScope currFuncName funcScope'
        return ()


closeScope :: Evaluator Int
closeScope = do
        decrementScope


stackPointerValue :: Evaluator Int
stackPointerValue = do
        currOff <- currentOffset
        return $ negate currOff


functionDefined :: String -> Evaluator Bool
functionDefined funcName = do
        progScope <- getProgramScope
        case M.lookup funcName progScope of
             Just fScope -> return True
             Nothing     -> return False


variableOffset :: String -> Evaluator Int
variableOffset name = do
        getOffset name


getBreak :: Evaluator Int
getBreak = do
        getOffset "@Break"


getContinue :: Evaluator Int
getContinue = do
        getOffset "@Continue"


setBreak :: Int -> Evaluator ()
setBreak labelNo = do
        store "@Break" labelNo


setContinue :: Int -> Evaluator ()
setContinue labelNo = do
        store "@Continue" labelNo


labelNum :: Evaluator Int
labelNum = do
        nextLabel


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        locScope <- getLocalScope scopeLevel funcScope
        return $ checkVar varName locScope


addVariable :: String -> Evaluator Int
addVariable varName = do
        currOff <- currentOffset
        store varName currOff
        incrementOffset currOff


{- Internal -}

getOffset :: String -> Evaluator Int
getOffset name = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        findOffset currFuncName scopeLevel name


findOffset :: String -> Int -> String -> Evaluator Int
findOffset func scope name =
        if scope == notFound
           then return notFound
           else do
                   offset <- lookUp func scope name
                   if offset == notFound
                      then findOffset func (pred scope) name
                      else return offset


lookUp :: String -> Int -> String -> Evaluator Int
lookUp func scope name = do
        progScope <- getProgramScope
        funcScope <- getFunctionScope func progScope
        locScope <- getLocalScope scope funcScope
        return $ getVar name locScope


store :: String -> Int -> Evaluator ()
store name value = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        locScope <- getLocalScope scopeLevel funcScope
        locScope' <- storeVariable name value locScope
        funcScope' <- updateFunctionScope scopeLevel locScope' funcScope
        updateProgramScope currFuncName funcScope'
        return ()


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
