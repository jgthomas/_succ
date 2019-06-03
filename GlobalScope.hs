
module GlobalScope (newGlobalScope,
                    declareFunction,
                    declareGlobal,
                    decParamCount,
                    decSeqNumber,
                    currentSeqNumber) where


import qualified Data.Map as M

import Evaluator            (Evaluator(Ev))
import Types                (GlobalScope(..), SymTab(globalScope))
import qualified FrameStack (currentFunction)


{- API -}

newGlobalScope :: GlobalScope
newGlobalScope = Gscope
                 0
                 M.empty
                 M.empty
                 M.empty


declareFunction :: String -> Int -> Evaluator ()
declareFunction funcName paramCount = do
        gscope <- getGlobalScope
        let gscope'  = addSymbol gscope funcName
            gscope'' = addParams gscope' funcName paramCount
        updateGlobalScope gscope''


declareGlobal :: String -> Evaluator ()
declareGlobal name = do
        gscope <- getGlobalScope
        updateGlobalScope $ addSymbol gscope name


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


addSymbol :: GlobalScope -> String -> GlobalScope
addSymbol gscope name =
        let n        = seqNum gscope
            declared = declarations gscope
            gscope'  = gscope { seqNum = n + 1 }
            gscope'' = gscope' { declarations = M.insert name n declared }
            in
        gscope''


addParams :: GlobalScope -> String -> Int -> GlobalScope
addParams gscope funcName paramCount =
        let paramTab = decParams gscope
            gscope'  = gscope { decParams = M.insert funcName paramCount paramTab }
            in
        gscope'
