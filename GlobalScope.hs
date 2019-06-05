
module GlobalScope (newGlobalScope,
                    declareFunction,
                    declareGlobal,
                    decParamCount,
                    decSeqNumber,
                    currentSeqNumber,
                    defineGlobal) where


import qualified Data.Map as M

import Evaluator            (Evaluator(Ev))
import Types                (GlobalScope(..), SymTab(globalScope))
import qualified FrameStack (currentFunction)


newGlobalScope :: GlobalScope
newGlobalScope = Gscope 0 M.empty M.empty M.empty


declareFunction :: String -> Int -> Evaluator ()
declareFunction funcName paramCount = do
        gscope <- getGlobalScope
        let gscope'  = addSymbol funcName gscope
            gscope'' = addParams funcName paramCount gscope'
        updateGlobalScope gscope''


declareGlobal :: String -> Evaluator ()
declareGlobal name = do
        gscope <- getGlobalScope
        updateGlobalScope $ addSymbol name gscope


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


defineGlobal :: String -> String -> Evaluator ()
defineGlobal varName varLabel = do
        gscope <- getGlobalScope
        updateGlobalScope $ addGlobal varName varLabel gscope


globalLabel :: String -> Evaluator (Maybe String)
globalLabel name = do
        gscope <- getGlobalScope
        return $ M.lookup name $ globalVars gscope


{- Internal -}

getGlobalScope :: Evaluator GlobalScope
getGlobalScope = Ev $ \symTab ->
        (globalScope symTab, symTab)


updateGlobalScope :: GlobalScope -> Evaluator ()
updateGlobalScope gscope = Ev $ \symTab ->
        ((), symTab { globalScope = gscope })


addGlobal :: String -> String -> GlobalScope -> GlobalScope
addGlobal n l s = s { globalVars = M.insert n l $ globalVars s }


addParams :: String -> Int -> GlobalScope -> GlobalScope
addParams n p s = s { decParams = M.insert n p $ decParams s }


addSymbol :: String -> GlobalScope -> GlobalScope
addSymbol name gs =
        let i   = seqNum gs
            gs' = gs { seqNum = i + 1 }
            in
        gs' { declarations = M.insert name i $ declarations gs' }
