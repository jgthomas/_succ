
module Declarations (newDecTable,
                     addDeclaration,
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


addDeclaration :: String -> Int -> Evaluator Declared
addDeclaration funcName paramCount = do
        insertDeclaration funcName paramCount


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

insertDeclaration :: String -> Int -> Evaluator Declared
insertDeclaration funcName paramCount = Ev $ \symTab ->
        let declared  = declarations symTab
            declared' = declFunc declared funcName paramCount
            symTab'   = symTab { declarations = declared' }
            in
        (declared', symTab')


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


declVar :: Declared -> String -> Declared
declVar table name = addSymbol table name


declFunc :: Declared -> String -> Int -> Declared
declFunc table name paramCount =
        let table'  = addSymbol table name
            table'' = addParams table' name paramCount
            in
        table''


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
