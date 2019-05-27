
module Declarations (newDecTable)
        where


import Data.Map as M


type SeqNums = M.Map String Int
type ParamCounts = M.Map String Int


data Declared = D { seqNum    :: Int
                  , declOrder :: SeqNums
                  , parameter :: ParamCounts }
              deriving Show

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
        checkDeclared seqTab name


paramCount :: Declared -> String -> Int
paramCount table name =
        let params = parameter table
            in
        getParamCount params name


{- Internal -}

getParamCount :: ParamCounts -> String -> Int
getParamCount counts name =
        case M.lookup name counts of
             Just n  -> n
             Nothing -> notFound


checkDeclared :: SeqNums -> String -> Bool
checkDeclared seq name =
        case M.lookup name seq of
             Just n  -> True
             Nothing -> False


notFound :: Int
notFound = -1
