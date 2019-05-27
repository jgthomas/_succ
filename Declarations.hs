
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


checkDeclared :: Declaration -> String -> Bool
checkDeclared (SymbolSeq seq) name = lookUp seq name


lookUp :: SeqNums -> String -> Bool
lookUp table name =
        case M.lookup name table of
             Just n  -> True
             Nothing -> False
