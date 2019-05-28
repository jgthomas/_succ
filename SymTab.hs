
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
               notFound,
               addDeclaration,
               declarationParamCount,
               declarationSequenceNumber,
               currentFuncSeqNumber,
               functionDefined,
               parameterDeclared)
        where


import qualified Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (SymTab(..),
              Declared(..),
              FuncState(..),
              FuncStates(..),
              LocalScope,
              FunctionScope,
              ProgramScope)
import Declarations (newDecTable,
                    insertDeclaration,
                    declarParamCount,
                    declarSeqNumber)
import SimpleStack (newStack,
                   currentFunction,
                   popFunctionName,
                   pushFunctionName)
import FunctionState (newFuncState,
                      getFunctionState,
                      setFunctionState,
                      addParam,
                      paramPos,
                      incrementArgCount,
                      resetArgs)


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


initFunction :: String -> Evaluator Bool
initFunction name = do
        pushFunctionName name
        newScopeRecord name
        newFuncState name
        progScope <- updateProgramScope name M.empty
        funcScope <- getFunctionScope name progScope
        funcScope' <- updateFunctionScope baseScope M.empty funcScope
        updateProgramScope name funcScope'
        return True


closeFunction :: Evaluator Bool
closeFunction = do
        popFunctionName


initScope :: Evaluator ProgramScope
initScope = do
        currFuncName <- currentFunction
        newScopeLevel <- incrementScope
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        funcScope' <- updateFunctionScope newScopeLevel M.empty funcScope
        updateProgramScope currFuncName funcScope'


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


setBreak :: Int -> Evaluator ProgramScope
setBreak labelNo = do
        store "@Break" labelNo


setContinue :: Int -> Evaluator ProgramScope
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


-- function state manipulation

addParameter :: String -> Evaluator FuncStates
addParameter paramName = do
        currFuncName <- currentFunction
        funcState <- getFunctionState currFuncName
        funcState' <- addParam paramName funcState
        setFunctionState currFuncName funcState'


parameterPosition :: String -> Evaluator Int
parameterPosition paramName = do
        currFuncName <- currentFunction
        funcState <- getFunctionState currFuncName
        paramPos paramName funcState


parameterDeclared :: String -> Evaluator Bool
parameterDeclared paramName = do
        currFuncName <- currentFunction
        funcState <- getFunctionState currFuncName
        pos <- paramPos paramName funcState
        if pos == notFound
           then return False
           else return True


nextArgumentPos :: Evaluator Int
nextArgumentPos = do
        currFuncName <- currentFunction
        funcState <- getFunctionState currFuncName
        funcState' <- incrementArgCount funcState
        setFunctionState currFuncName funcState'
        return $ argCount funcState


resetArguments :: Evaluator FuncStates
resetArguments = do
        currFuncName <- currentFunction
        funcState <- getFunctionState currFuncName
        funcState' <- resetArgs funcState
        setFunctionState currFuncName funcState'


-- declarations

addDeclaration :: String -> Int -> Evaluator Declared
addDeclaration funcName paramCount = do
        insertDeclaration funcName paramCount


declarationParamCount :: String -> Evaluator Int
declarationParamCount funcName = do
        declarParamCount funcName


declarationSequenceNumber :: String -> Evaluator Int
declarationSequenceNumber funcName = do
        declarSeqNumber funcName


currentFuncSeqNumber :: Evaluator Int
currentFuncSeqNumber = do
        currFuncName <- currentFunction
        declarSeqNumber currFuncName


{-
- Internal functions
-}


-- lookup and storage

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


store :: String -> Int -> Evaluator ProgramScope
store name value = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        progScope <- getProgramScope
        funcScope <- getFunctionScope currFuncName progScope
        locScope <- getLocalScope scopeLevel funcScope
        locScope' <- storeVariable name value locScope
        funcScope' <- updateFunctionScope scopeLevel locScope' funcScope
        updateProgramScope currFuncName funcScope'


-- scope variables viewing and editing

getLocalScope :: Int -> FunctionScope -> Evaluator LocalScope
getLocalScope scopeLevel funcScope = Ev $ \symTab ->
        case M.lookup scopeLevel funcScope of
             Just locScope -> (locScope, symTab)
             Nothing       -> error "No scope defined for function"


getFunctionScope :: String -> ProgramScope -> Evaluator FunctionScope
getFunctionScope funcName progScope = Ev $ \symTab ->
        case M.lookup funcName progScope of
             Just funcScope -> (funcScope, symTab)
             Nothing        -> error "No function scopes defined"


getProgramScope :: Evaluator ProgramScope
getProgramScope = Ev $ \symTab ->
        let scopes = scopesData symTab
            in
        (scopes, symTab)


storeVariable :: String -> Int -> LocalScope -> Evaluator LocalScope
storeVariable varName value locScope =
        let locScope' = M.insert varName value locScope
            in
        return locScope'


updateFunctionScope :: Int -> LocalScope -> FunctionScope -> Evaluator FunctionScope
updateFunctionScope scopeLevel locScope funcScope =
        let funcScope' = M.insert scopeLevel locScope funcScope
            in
        return funcScope'


updateProgramScope :: String -> FunctionScope -> Evaluator ProgramScope
updateProgramScope funcName funcScope = Ev $ \symTab ->
        let scopes = scopesData symTab
            symTab' = symTab { scopesData = M.insert funcName funcScope scopes }
            scopes' = scopesData symTab'
            in
        (scopes', symTab')


checkVar :: String -> LocalScope -> Bool
checkVar varName varMap =
        case M.lookup varName varMap of
             Just v  -> True
             Nothing -> False


getVar :: String -> LocalScope -> Int
getVar varName varMap =
        let value = M.lookup varName varMap
            in case value of
                    Just v  -> v
                    Nothing -> notFound


-- scope level adjustment and reporting

incrementScope :: Evaluator Int
incrementScope = do
        stepScope succ


decrementScope :: Evaluator Int
decrementScope = do
        stepScope pred


stepScope :: (Int -> Int) -> Evaluator Int
stepScope func = do
        currFuncName <- currentFunction
        scopeLevel <- findScope currFuncName
        switchScope currFuncName $ func scopeLevel


switchScope :: String -> Int -> Evaluator Int
switchScope name newScopeLevel = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            symTab' = symTab { scopeLevels = M.insert name newScopeLevel scopes }
            in
        (newScopeLevel, symTab')


findScope :: String -> Evaluator Int
findScope name = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            scopeLevel = M.lookup name scopes
            in
        case scopeLevel of
             Just scope -> (scope, symTab)
             Nothing    -> error $ "No scopes defined for function " ++ name


-- querying and altering symtab state

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


newScopeRecord :: String -> Evaluator Int
newScopeRecord name = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            symTab' = symTab { scopeLevels = M.insert name baseScope scopes }
            in
        (baseScope, symTab')


-- convenience 'value' functions

notFound :: Int
notFound = -1

memOffsetSize :: Int
memOffsetSize = (-8)

firstLabel :: Int
firstLabel = 1

baseScope :: Int
baseScope = 0
