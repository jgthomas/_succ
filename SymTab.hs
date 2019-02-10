
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
               closeFunction) where


import Lexer
import Parser
import SimpleStack
import qualified Data.Map as M
import Control.Monad (liftM, ap)


data Change = INC
            | DEC
            deriving Eq


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope


data SymTab = Tab { scope     :: Int
                  , labelNo   :: Int
                  , offset    :: Int
                  , funcNames :: Stack String
                  , funcScope :: M.Map String Int
                  , funcVars  :: M.Map String (M.Map Int (M.Map String Int))
                  , variables :: M.Map Int (M.Map String Int)}
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
        (Ev act) >>= k = Ev $
            \symTab ->
                    let (x, symTab') = act symTab
                        (Ev act')    = k x
                    in act' symTab'
        return x = Ev (\symTab -> (x, symTab))


{-
- Exported API functions
-}

newSymTab :: SymTab
newSymTab = Tab
            (-1)
            firstLabel
            memOffsetSize
            newStack
            M.empty
            M.empty
            M.empty


initFunction :: String -> Evaluator Bool
initFunction name = do
        pushFunctionName name
        newScopeRecord name
        progScope <- sFunc name M.empty
        funcScope <- fScope name progScope
        sScope 0 M.empty funcScope
        return True


closeFunction :: Evaluator Bool
closeFunction = do
        popFunctionName


initScope :: Evaluator Bool
initScope = do
        changeScope increment
        currScope <- currentScope
        funcScope <- functionScopes
        storeScope currScope M.empty
        return True


--initScope :: Evaluator ProgramScope
--initScope = do
--        currFunc <- currentFunction
--        newScope <- incrementScope
--        progScope <- pScopes
--        funcScope <- fScope currFunc progScope
--        funcScope' <- sScope newScope M.empty funcScope
--        sFunc currFunc funcScope'


closeScope :: Evaluator Bool
closeScope = do
        changeScope decrement


--closeScope :: Evaluator Int
--closeScope = do
--        decrementScope


stackPointerValue :: Evaluator Int
stackPointerValue = do
        currOff <- currentOffset
        return $ negate currOff


variableOffset :: String -> Evaluator Int
variableOffset varName = do
        currScope <- currentScope
        findOffset currScope varName


--variableOffset :: String -> Evaluator Int
--variableOffset name = do
--        getOffset name


getBreak :: Evaluator Int
getBreak = do
        currScope <- currentScope
        findOffset currScope "@Break"


--getBreak :: Evaluator Int
--getBreak = do
--        getOffset "@Break"


getContinue :: Evaluator Int
getContinue = do
        currScope <- currentScope
        findOffset currScope "@Continue"


--getContinue :: Evaluator Int
--getContinue = do
--        getOffset "@Continue"
--

setBreak :: Int -> Evaluator FunctionScope
setBreak labelNo = do
        store "@Break" labelNo


--setBreak :: Int -> Evaluator ProgramScope
--setBreak labelNo = do
--        store "@Break" labelNo


setContinue :: Int -> Evaluator FunctionScope
setContinue labelNo = do
        store "@Continue" labelNo


--setContinue :: Int -> Evaluator ProgramScope
--setContinue labelNo = do
--        store "@Continue" labelNo


labelNum :: Evaluator Int
labelNum = do
        nextLabel


checkVariable :: String -> Evaluator Bool
checkVariable varName = do
        currScope <- currentScope
        funcScope <- functionScopes
        locScope <- localScope currScope funcScope
        return $ checkVar varName locScope


--checkVariable :: String -> Evaluator Bool
--checkVariable varName = do
--        currFunc <- currentFunction
--        currScope <- findScope currFunc
--        progScope <- pScopes
--        funcScope <- fScope currFunc progScope
--        locScope <- lScope currScope funcScope
--        return $ checkVar varName locScope


addVariable :: String -> Evaluator Int
addVariable varName = do
        currOff <- currentOffset
        store varName currOff
        incrementOffset currOff


{-
- Internal functions
-}


--getOffset :: String -> Evaluator Int
--getOffset name = do
--        currFunc <- currentFunction
--        currScope <- findScope currFunc
--        findOffset currFunc currScope name


findOffset :: Int -> String -> Evaluator Int
findOffset currScope varName =
        if currScope == notFound
           then error $ "Undefined variable: '" ++ varName ++ "'"
           else do
                   offset <- lookUp currScope varName
                   if offset == notFound
                      then findOffset (currScope-1) varName
                      else return offset


--findOffset :: String -> Int -> String -> Evaluator Int
--findOffset func scope name =
--        if scope == notFound
--           then error $ "Undefined variable: '" ++ name ++ "'"
--           else do
--                   offset <- lookUp func scope name
--                   if offset == notFound
--                      then findOffset func (scope-1) name
--                      else return offset
--

lookUp :: Int -> String -> Evaluator Int
lookUp currScope name = do
        funcScope <- functionScopes
        locScope <- localScope currScope funcScope
        return $ getVar name locScope


--lookUp :: String -> Int -> String -> Evaluator Int
--lookUp func scope name = do
--        progScope <- pScopes
--        funcScope <- fScope func progScope
--        locScope <- lScope scope funcScope
--        return $ getVar name locScope


store :: String -> Int -> Evaluator FunctionScope
store name value = do
        currScope <- currentScope
        funcScope <- functionScopes
        locScope <- localScope currScope funcScope
        storeScope currScope (storeVariable name value locScope)


--store :: String -> Int -> Evaluator ProgramScope
--store name value = do
--        currFunc <- currentFunction
--        currScope <- findScope currFunc
--        progScope <- pScopes
--        funcScope <- fScope currFunc progScope
--        locScope <- lScope currScope funcScope
--        locScope' <- sVariable name value locScope
--        funcScope' <- sScope currScope locScope' funcScope
--        sFunc currFunc funcScope'


-- new versions for after switch

lScope :: Int -> FunctionScope -> Evaluator LocalScope
lScope currScope funcScope = Ev $ \symTab ->
        case M.lookup currScope funcScope of
             Just value -> (value, symTab)
             Nothing    -> error "No scope defined for function"


fScope :: String -> ProgramScope -> Evaluator FunctionScope
fScope name progScope = Ev $ \symTab ->
        case M.lookup name progScope of
             Just v  -> (v, symTab)
             Nothing -> error "No function scopes defined"


pScopes :: Evaluator ProgramScope
pScopes = Ev $ \symTab ->
        let scopeData = funcVars symTab
            in
        (scopeData, symTab)


sVariable :: String -> Int -> LocalScope -> Evaluator LocalScope
sVariable varName value locScope =
        let locScope' = M.insert varName value locScope
            in
        return locScope'


sScope :: Int -> LocalScope -> FunctionScope -> Evaluator FunctionScope
sScope currScope locScope funcScope =
        let funcScope' = M.insert currScope locScope funcScope
            in
        return funcScope'


sFunc :: String -> FunctionScope -> Evaluator ProgramScope
sFunc name funcScope = Ev $ \symTab ->
        let scopeData = funcVars symTab
            symTab' = symTab { funcVars = M.insert name funcScope scopeData }
            scopeData' = funcVars symTab'
            in
        (scopeData', symTab')


-- end new versions


-- old versions

storeVariable :: String -> Int -> LocalScope -> LocalScope
storeVariable varName value locScope =
        let locScope' = M.insert varName value locScope
            in
        locScope'


storeScope :: Int -> LocalScope -> Evaluator FunctionScope
storeScope currScope locScope = Ev $ \symTab ->
        let scopeData = variables symTab
            symTab' = symTab { variables = M.insert currScope locScope scopeData }
            in
        (scopeData, symTab')



functionScopes :: Evaluator FunctionScope
functionScopes = Ev $ \symTab ->
        let scopeData = variables symTab
            in
        (scopeData, symTab)

localScope :: Int -> FunctionScope -> Evaluator LocalScope
localScope currScope funcScope = Ev $ \symTab ->
        case M.lookup currScope funcScope of
             Just value -> (value, symTab)
             Nothing    -> error "No scope defined for function"

-- end old versions


checkVar :: String -> LocalScope -> Bool
checkVar varName varMap =
        case M.lookup varName varMap of
             Just value -> True
             Nothing    -> False


getVar :: String -> LocalScope -> Int
getVar varName varMap =
        let value = M.lookup varName varMap
            in case value of
                    Just v  -> v
                    Nothing -> notFound


-- new scope functions

incrementScope :: Evaluator Int
incrementScope = do
        stepScope increment


decrementScope :: Evaluator Int
decrementScope = do
        stepScope decrement


--getScope :: Evaluator Int
--getScope = do
--        currFunc <- currentFunction
--        findScope currFunc


stepScope :: Int -> Evaluator Int
stepScope change = do
        currFunc <- currentFunction
        currScope <- findScope currFunc
        switchScope currFunc (currScope + change)


switchScope :: String -> Int -> Evaluator Int
switchScope name newScope = Ev $ \symTab ->
        let scopes = funcScope symTab
            symTab' = symTab { funcScope = M.insert name newScope scopes }
            in
        (newScope, symTab')


findScope :: String -> Evaluator Int
findScope name = Ev $ \symTab ->
        let scopes = funcScope symTab
            currScope = M.lookup name scopes
            in
        case currScope of
             Just scope -> (scope, symTab)
             Nothing    -> error $ "No scopes defined for function " ++ name

-- end new scope functions


currentFunction :: Evaluator String
currentFunction = Ev $ \symTab ->
        let nameStack = funcNames symTab
            currFunc  = stackPeek nameStack
            in
        (currFunc, symTab)


currentScope :: Evaluator Int
currentScope = Ev $ \symTab ->
        let currScope = scope symTab
            in
        (currScope, symTab)


currentOffset :: Evaluator Int
currentOffset = Ev $ \symTab ->
        let currOff = offset symTab
            in
        (currOff, symTab)


incrementOffset :: Int -> Evaluator Int
incrementOffset currOff = Ev $ \symTab ->
        let symTab' = symTab { offset = currOff + (-8) }
            in
        (currOff, symTab')


changeScope :: Int -> Evaluator Bool
changeScope n = Ev $ \symTab ->
        let symTab' = symTab { scope = scope symTab + n }
            in
        (True, symTab')


nextLabel :: Evaluator Int
nextLabel = Ev $ \symTab ->
        let num = labelNo symTab
            symTab' = symTab { labelNo = num + increment }
            in
        (num, symTab')


popFunctionName :: Evaluator Bool
popFunctionName = Ev $ \symTab ->
        let names = funcNames symTab
            symTab' = symTab { funcNames = stackPop names }
            in
        (True, symTab')


pushFunctionName :: String -> Evaluator Bool
pushFunctionName name = Ev $ \symTab ->
        let names = funcNames symTab
            symTab' = symTab { funcNames = stackPush name names }
            in
        (True, symTab')


newScopeRecord :: String -> Evaluator Int
newScopeRecord name = Ev $ \symTab ->
        let scopes = funcScope symTab
            symTab' = symTab { funcScope = M.insert name 0 scopes }
            in
        (0, symTab')


notFound :: Int
notFound = -1

increment :: Int
increment = 1

decrement :: Int
decrement = (-1)

memOffsetSize :: Int
memOffsetSize = (-8)

firstLabel :: Int
firstLabel = 1
