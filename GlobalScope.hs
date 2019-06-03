
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
        declareFunc funcName paramCount


declareGlobal :: String -> Evaluator ()
declareGlobal name = do
        declareVar name


decParamCount :: String -> Evaluator (Maybe Int)
decParamCount funcName = do
        paramCount funcName


decSeqNumber :: String -> Evaluator (Maybe Int)
decSeqNumber funcName = do
        seqNumber funcName


currentSeqNumber :: Evaluator (Maybe Int)
currentSeqNumber = do
        currFuncName <- FrameStack.currentFunction
        seqNumber currFuncName


defineGlobal :: String -> String -> Evaluator ()
defineGlobal varName varLabel = do
        addGlobal varName varLabel


{- Internal -}


addGlobal :: String -> String -> Evaluator ()
addGlobal varName varLabel = Ev $ \symTab ->
        let gscope  = globalScope symTab
            gvars   = globalVars gscope
            gscope' = gscope { globalVars = M.insert varName varLabel gvars }
            symTab' = symTab { globalScope = gscope' }
            in
        ((), symTab')


declareFunc :: String -> Int -> Evaluator ()
declareFunc funcName paramCount = Ev $ \symTab ->
        let gscope   = globalScope symTab
            gscope'  = addSymbol gscope funcName
            gscope'' = addParams gscope' funcName paramCount
            symTab'  = symTab { globalScope = gscope'' }
            in
        ((), symTab')


declareVar :: String -> Evaluator ()
declareVar varName = Ev $ \symTab ->
        let gscope  = globalScope symTab
            gscope' = addSymbol gscope varName
            symTab' = symTab { globalScope = gscope' }
            in
        ((), symTab')


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


paramCount :: String -> Evaluator (Maybe Int)
paramCount funcName = Ev $ \symTab ->
        let gscope = globalScope symTab
            params = decParams gscope
            in
        case M.lookup funcName params of
             Just n  -> (Just n, symTab)
             Nothing -> (Nothing, symTab)


seqNumber :: String -> Evaluator (Maybe Int)
seqNumber funcName = Ev $ \symTab ->
        let gscope = globalScope symTab
            seqTab = declarations gscope
            in
        case M.lookup funcName seqTab of
             Just n  -> (Just n, symTab)
             Nothing -> (Nothing, symTab)
