
module GlobalScope (newGlobalScope,
                    declareFunction,
                    declareGlobal,
                    decParamCount,
                    decSeqNumber,
                    currentSeqNumber,
                    globalLabel,
                    checkVarDefined,
                    getUndefined,
                    defineGlobal) where


import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (toList)

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
        return $ M.lookup funcName $ funcParams gscope


decSeqNumber :: String -> Evaluator (Maybe Int)
decSeqNumber name = do
        gscope <- getGlobalScope
        return $ M.lookup name $ declarations gscope


currentSeqNumber :: Evaluator (Maybe Int)
currentSeqNumber = do
        currFunc <- FrameStack.currentFunction
        seqnum   <- decSeqNumber currFunc
        return seqnum


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


getUndefined :: Evaluator [String]
getUndefined = do
        gscope <- getGlobalScope
        let definedSet  = definedVars gscope
            declaredSet = M.keysSet $ declaredVars gscope
            undefined   = toList $ S.difference declaredSet definedSet
        return $ M.elems $ M.filterWithKey (\k _ -> elem k undefined) $ declaredVars gscope


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
addParams n p s = s { funcParams = M.insert n p $ funcParams s }


varAsDefined :: String -> GlobalScope -> GlobalScope
varAsDefined n s = s { definedVars = S.insert n $ definedVars s }


addSymbol :: String -> GlobalScope -> GlobalScope
addSymbol name gs =
        let i   = seqNum gs
            gs' = gs { seqNum = i + 1 }
            in
        gs' { declarations = M.insert name i $ declarations gs' }
