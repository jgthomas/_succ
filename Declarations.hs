
module Declarations (newDecTable,
                     isDeclared,
                     paramNum,
                     seqNumber,
                     declVar,
                     declFunc)
        where


import Data.Map as M

import Types (Declared(..), SeqNums, ParamCounts)


{- API -}

newDecTable :: Declared
newDecTable = D
              0
              M.empty
              M.empty


isDeclared :: Declared -> String -> Bool
isDeclared table name =
        let seqTab = declOrder table
            in
        if (getSeqNum seqTab name) == notFound
           then False
           else True


paramNum :: Declared -> String -> Int
paramNum table name =
        let params = parameter table
            in
        getParamCount params name


seqNumber :: Declared -> String -> Int
seqNumber table name =
        let seqTab = declOrder table
            in
        getSeqNum seqTab name


declVar :: Declared -> String -> Declared
declVar table name = addSymbol table name


declFunc :: Declared -> String -> Int -> Declared
declFunc table name paramCount =
        let table'  = addSymbol table name
            table'' = addParams table' name paramCount
            in
        table''


{- Internal -}

getParamCount :: ParamCounts -> String -> Int
getParamCount counts name =
        case M.lookup name counts of
             Just n  -> n
             Nothing -> notFound


getSeqNum :: SeqNums -> String -> Int
getSeqNum seq name =
        case M.lookup name seq of
             Just n  -> n
             Nothing -> notFound


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
            table'   = table { parameter = M.insert name paramCount paramTab }
            in
        table'


notFound :: Int
notFound = -1
