
module LexDat (LexDat(..)) where


import Tokens (Token)


data LexDat = LexDat { tok  :: Token
                     , line :: Int }
            deriving (Show, Eq)


