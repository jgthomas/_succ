
module SymTab (Evaluator(..),
               newSymTab,
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


import Lexer
import Parser
import SimpleStack
import qualified Data.Map as M
import qualified Data.Map.Ordered as O
import Control.Monad (liftM, ap)


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope

type FuncParams = M.Map String Int


{-
- EXTERNAL DEPENDENCY
-
- https://hackage.haskell.org/package/ordered-containers
- https://github.com/dmwit/ordered-containers
-
- Ordered map: allows querying declaration (1) for its parameter count
- and (2) for the index order of when it was added
-
- (1) declarParamCount name:
- access by name, gets the parameter count
-
- e.g. the parameter count at each declaration/definition should be the same
-
- (2) declarSeqNumber name
- access by name, gets the index number of when this declaration was added
-
- e.g. if dog() calls cat(), then cat() needs to have been declared
- *before* dog(), and should thus have a lower index number
-
-}
type Declarations = O.OMap String Int


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


type FuncStates = M.Map String FuncState


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
data SymTab = Tab { labelNo      :: Int
                  , offset       :: Int
                  , nameStack    :: Stack String
                  , declarations :: Declarations
                  , funcStates   :: FuncStates
                  , scopeLevels  :: M.Map String Int
                  , scopesData   :: ProgramScope }
            deriving Show


newtype Evaluator a = Ev (SymTab -> (a, SymTab))


{-
-  Monad is now defined as a subclass of applicative
-  and so new monad definitions require that they also
-  be specified as applicatives (and functors, of which
-  applicatives are a subclass)
-
-  See: https://stackoverflow.com/a/34641501/5671759
-}
instance Functor Evaluator where
        fmap = liftM


instance Applicative Evaluator where
        pure  = return
        (<*>) = ap


instance Monad Evaluator where
        -- return :: a -> ma
        return x = Ev (\symTab -> (x, symTab))

        -- (>>=)  :: ma -> (a -> mb) -> mb
        (Ev act) >>= k = Ev $ \symTab ->
                let (x, symTab') = act symTab
                    (Ev act')    = k x
                    in
                act' symTab'


{-
- Exported API functions
-}

newSymTab :: SymTab
newSymTab = Tab
            firstLabel
            memOffsetSize
            newStack
            O.empty
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

addDeclaration :: String -> Int -> Evaluator Declarations
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

insertDeclaration :: String -> Int -> Evaluator Declarations
insertDeclaration funcName paramCount = Ev $ \symTab ->
        let declar = declarations symTab
            declar' = (O.|>) declar (funcName,paramCount)
            symTab' = symTab { declarations = declar' }
            in
        (declar', symTab')


declarParamCount :: String -> Evaluator Int
declarParamCount funcName = Ev $ \symTab ->
        let declar = declarations symTab
            in case O.lookup funcName declar of
                    Just n  -> (n, symTab)
                    Nothing -> (notFound, symTab)


declarSeqNumber :: String -> Evaluator Int
declarSeqNumber funcName = Ev $ \symTab ->
        let declar = declarations symTab
            in case O.findIndex funcName declar of
                    Just n  -> (n, symTab)
                    Nothing -> (notFound, symTab)


-- convenience 'value' functions

notFound :: Int
notFound = -1

memOffsetSize :: Int
memOffsetSize = (-8)

firstLabel :: Int
firstLabel = 1

baseScope :: Int
baseScope = 0
