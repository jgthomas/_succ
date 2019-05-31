
module Declarations (newDecTable,
                     declareFunction,
                     declareGlobal,
                     decParamCount,
                     decSeqNumber,
                     currentSeqNumber) where


import Data.Map as M

import Evaluator (Evaluator(Ev))
import Types (Declared(..), SymTab(declarations), SeqNums, ParamCounts)
import SimpleStack (currentFunction)


{- API -}

newDecTable :: Declared
newDecTable = D
              0
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
        currFuncName <- currentFunction
        seqNumber currFuncName


{- Internal -}

declareFunc :: String -> Int -> Evaluator ()
declareFunc funcName paramCount = Ev $ \symTab ->
        let declared   = declarations symTab
            declared'  = addSymbol declared funcName
            declared'' = addParams declared' funcName paramCount
            symTab'    = symTab { declarations = declared'' }
            in
        ((), symTab')


declareVar :: String -> Evaluator ()
declareVar name = Ev $ \symTab ->
        let declared  = declarations symTab
            declared' = addSymbol declared name
            symTab'   = symTab { declarations = declared' }
            in
        ((), symTab')


paramCount :: String -> Evaluator (Maybe Int)
paramCount funcName = Ev $ \symTab ->
        let declared = declarations symTab
            params   = parameter declared
            in
        case M.lookup funcName params of
             Just n  -> (Just n, symTab)
             Nothing -> (Nothing, symTab)


seqNumber :: String -> Evaluator (Maybe Int)
seqNumber funcName = Ev $ \symTab ->
        let declared = declarations symTab
            seqTab   = declOrder declared
            in
        case M.lookup funcName seqTab of
             Just n  -> (Just n, symTab)
             Nothing -> (Nothing, symTab)


addSymbol :: Declared -> String -> Declared
addSymbol table name =
        let seq      = seqNum table
            seqTable = declOrder table
            table'   = table { seqNum = seq + 1 }
            table''  = table' { declOrder = M.insert name seq seqTable }
            in
        table''


addParams :: Declared -> String -> Int -> Declared
addParams table name paramCount =
        let paramTab = parameter table
            in
        case M.lookup name paramTab of
             Just n  -> table
             Nothing ->
                     let table' = table { parameter = M.insert name paramCount paramTab }
                         in
                     table'
