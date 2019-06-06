
module GlobalScope (newGlobalScope,
                    declareFunction,
                    declareGlobal,
                    decParamCount,
                    decSeqNumber,
                    currentSeqNumber,
                    globalLabel,
                    checkVarDefined,
                    defineGlobal) where


import qualified Data.Map as M
import qualified Data.Set as S

import Evaluator            (Evaluator(Ev))
import Types                (SymTab(globalScope), GlobalScope(..))
import qualified FrameStack (currentFunction)


newGlobalScope :: GlobalScope
newGlobalScope = Gscope 0 M.empty M.empty M.empty S.empty


declareFunction :: String -> Int -> Evaluator ()
declareFunction funcName paramCount = do
        gscope <- getGlobalScope
        let gscope'  = addSymbol funcName gscope
            gscope'' = addParams funcName paramCount gscope'
        updateGlobalScope gscope''


declareGlobal :: String -> String -> Evaluator ()
declareGlobal name label = do
        gscope <- getGlobalScope
        let gscope'  = addSymbol name gscope
            gscope'' = addGlobal name label gscope'
        updateGlobalScope gscope''


decParamCount :: String -> Evaluator (Maybe Int)
decParamCount funcName = do
        gscope <- getGlobalScope
        return $ M.lookup funcName $ decParams gscope


decSeqNumber :: String -> Evaluator (Maybe Int)
decSeqNumber name = do
        gscope <- getGlobalScope
        return $ M.lookup name $ declarations gscope


currentSeqNumber :: Evaluator (Maybe Int)
currentSeqNumber = do
        currFunc <- FrameStack.currentFunction
        gscope   <- getGlobalScope
        return $ M.lookup currFunc $ declarations gscope


defineGlobal :: String -> Evaluator ()
defineGlobal name = do
        gscope <- getGlobalScope
        updateGlobalScope $ varAsDefined name gscope


checkVarDefined :: String -> Evaluator Bool
checkVarDefined name = do
        gscope <- getGlobalScope
        return $ S.member name $ definedVars gscope


globalLabel :: String -> Evaluator (Maybe String)
globalLabel name = do
        gscope <- getGlobalScope
        return $ M.lookup name $ declaredVars gscope


{- Internal -}

getGlobalScope :: Evaluator GlobalScope
getGlobalScope = Ev $ \symTab ->
        (globalScope symTab, symTab)


updateGlobalScope :: GlobalScope -> Evaluator ()
updateGlobalScope gscope = Ev $ \symTab ->
        ((), symTab { globalScope = gscope })


addGlobal :: String -> String -> GlobalScope -> GlobalScope
addGlobal n l s = s { declaredVars = M.insert n l $ declaredVars s }


addParams :: String -> Int -> GlobalScope -> GlobalScope
addParams n p s = s { decParams = M.insert n p $ decParams s }


varAsDefined :: String -> GlobalScope -> GlobalScope
varAsDefined n s = s { definedVars = S.insert n $ definedVars s }


addSymbol :: String -> GlobalScope -> GlobalScope
addSymbol name gs =
        let i   = seqNum gs
            gs' = gs { seqNum = i + 1 }
            in
        gs' { declarations = M.insert name i $ declarations gs' }
