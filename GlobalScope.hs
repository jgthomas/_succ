
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

import Evaluator            (Evaluator(Ev))
import Types                (SymTab(globalScope), GlobalScope(..))
import qualified FrameStack (currentFunction)


newGlobalScope :: GlobalScope
newGlobalScope = Gscope 0 M.empty M.empty M.empty S.empty


decParamCount :: String -> Evaluator (Maybe Int)
decParamCount name = lookUp name funcParams


decSeqNumber :: String -> Evaluator (Maybe Int)
decSeqNumber name = lookUp name funcDecSeq


globalLabel :: String -> Evaluator (Maybe String)
globalLabel name = lookUp name declaredVars


currentSeqNumber :: Evaluator (Maybe Int)
currentSeqNumber = do
        currFunc <- FrameStack.currentFunction
        decSeqNumber currFunc


checkVarDefined :: String -> Evaluator Bool
checkVarDefined name = S.member name . definedVars <$> getGlobalScope


declareFunction :: String -> Int -> Evaluator ()
declareFunction funcName paramCount = do
        gscope <- getGlobalScope
        let gscope'  = addSymbol funcName gscope
            gscope'' = addParams funcName paramCount gscope'
        updateGlobalScope gscope''


declareGlobal :: String -> String -> Evaluator ()
declareGlobal name label = do
        gscope <- getGlobalScope
        updateGlobalScope $ addGlobal name label gscope


defineGlobal :: String -> Evaluator ()
defineGlobal name = do
        gscope <- getGlobalScope
        updateGlobalScope $ varAsDefined name gscope


getUndefined :: Evaluator [String]
getUndefined = do
        gscope <- getGlobalScope
        let definedSet  = definedVars gscope
            declaredSet = M.keysSet $ declaredVars gscope
            undefined   = S.difference declaredSet definedSet
        return $ M.elems $ M.filterWithKey (\k _ -> k `elem` undefined) $ declaredVars gscope


{- Internal -}

getGlobalScope :: Evaluator GlobalScope
getGlobalScope = Ev $ \symTab ->
        (globalScope symTab, symTab)


updateGlobalScope :: GlobalScope -> Evaluator ()
updateGlobalScope gscope = Ev $ \symTab ->
        ((), symTab { globalScope = gscope })


lookUp :: (Ord a) => a -> (GlobalScope -> M.Map a b) -> Evaluator (Maybe b)
lookUp name f = M.lookup name . f <$> getGlobalScope


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
        gs' { funcDecSeq = M.insert name i $ funcDecSeq gs' }
