
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

import SymbolTable (SymTab(..))
import Evaluator (Evaluator(Ev))
import FunctionState (FuncState(..), FuncStates(..))
import Scopes (LocalScope, FunctionScope, ProgramScope)
import qualified Declarations as Dec
import SimpleStack (newStack, stackPeek, stackPop, stackPush)


{- API -}

newSymTab :: SymTab
newSymTab = Tab
            firstLabel
            memOffsetSize
            newStack
            Dec.newDecTable
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

addDeclaration :: String -> Int -> Evaluator Dec.Declared
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

currentFunction :: Evaluator String
currentFunction = Ev $ \symTab ->
        let stack = nameStack symTab
            currFuncName = stackPeek stack
            in
        (currFuncName, symTab)


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


popFunctionName :: Evaluator Bool
popFunctionName = Ev $ \symTab ->
        let stack = nameStack symTab
            symTab' = symTab { nameStack = stackPop stack }
            in
        (True, symTab')


pushFunctionName :: String -> Evaluator Bool
pushFunctionName funcName = Ev $ \symTab ->
        let stack = nameStack symTab
            symTab' = symTab { nameStack = stackPush funcName stack }
            in
        (True, symTab')


newScopeRecord :: String -> Evaluator Int
newScopeRecord name = Ev $ \symTab ->
        let scopes = scopeLevels symTab
            symTab' = symTab { scopeLevels = M.insert name baseScope scopes }
            in
        (baseScope, symTab')


-- function state

makeFuncState :: FuncState
makeFuncState = Fs 0 0 M.empty


newFuncState :: String -> Evaluator String
newFuncState funcName = Ev $ \symTab ->
        let states = funcStates symTab
            symTab' = symTab { funcStates = M.insert funcName makeFuncState states }
            in
        (funcName, symTab')


getFunctionState :: String -> Evaluator FuncState
getFunctionState funcName = Ev $ \symTab ->
        let states = funcStates symTab
            in
        case M.lookup funcName states of
             Just state -> (state, symTab)
             Nothing    -> error $ "No state defined for: " ++ funcName


setFunctionState :: String -> FuncState -> Evaluator FuncStates
setFunctionState funcName funcState = Ev $ \symTab ->
        let states = funcStates symTab
            symTab' = symTab { funcStates = M.insert funcName funcState states }
            states' = funcStates symTab
            in
        (states', symTab')


addParam :: String -> FuncState -> Evaluator FuncState
addParam paramName funcState =
        let params = parameters funcState
            pos = paramCount funcState
            funcState' = funcState { paramCount = pos + 1 }
            funcState'' = funcState' { parameters = M.insert paramName pos params }
            in
        return funcState''


paramPos :: String -> FuncState -> Evaluator Int
paramPos paramName funcState =
        let params = parameters funcState
            in
        case M.lookup paramName params of
             Just pos -> return pos
             Nothing  -> return notFound


incrementArgCount :: FuncState -> Evaluator FuncState
incrementArgCount funcState =
        let count = argCount funcState
            funcState' = funcState { argCount = count + 1 }
            in
        return funcState'


resetArgs :: FuncState -> Evaluator FuncState
resetArgs funcState =
        let funcState' = funcState { argCount = 0 }
            in
        return funcState'


-- declarations

insertDeclaration :: String -> Int -> Evaluator Dec.Declared
insertDeclaration funcName paramCount = Ev $ \symTab ->
        let declared  = declarations symTab
            declared' = Dec.declFunc declared funcName paramCount
            symTab'   = symTab { declarations = declared' }
            in
        (declared', symTab')


declarParamCount :: String -> Evaluator Int
declarParamCount funcName = Ev $ \symTab ->
        let declared = declarations symTab
            count = Dec.paramNum declared funcName
            in
        (count, symTab)


declarSeqNumber :: String -> Evaluator Int
declarSeqNumber funcName = Ev $ \symTab ->
        let declaration = declarations symTab
            seqNum = Dec.seqNumber declaration funcName
            in
        (seqNum, symTab)


-- convenience 'value' functions

notFound :: Int
notFound = -1

memOffsetSize :: Int
memOffsetSize = (-8)

firstLabel :: Int
firstLabel = 1

baseScope :: Int
baseScope = 0
