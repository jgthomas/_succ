
module Declarations (Declaration(..),
                     newDecTable)
        where


import Data.Map as M


type SeqNums = M.Map String Int
type ParamCounts = M.Map String Int


data Declaration = SymbolSeq SeqNums
                 | ParamCount ParamCounts
                 deriving Show


data Declared = D { seqNum    :: Int
                  , declOrder :: Declaration
                  , parameter :: Declaration }
              deriving Show

{- API -}

newDecTable :: Declared
newDecTable = D
              0
              (SymbolSeq M.empty)
              (ParamCount M.empty)


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

getParamCount :: Declaration -> String -> Int
getParamCount (ParamCount counts) name =
        case M.lookup name counts of
             Just n  -> n
             Nothing -> notFound


checkDeclared :: Declaration -> String -> Bool
checkDeclared (SymbolSeq seq) name =
        case M.lookup name seq of
             Just n  -> True
             Nothing -> False


notFound :: Int
notFound = -1
