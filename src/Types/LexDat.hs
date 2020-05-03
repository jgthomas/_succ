{-|
Module       : LexDat
Description  : Holding data type for lexer stage

Data type used for the Lexer stage of succ.
-}
module Types.LexDat where


import Types.Tokens (Token)


-- | LexDat state data type
data LexDat = LexDat { tok  :: Token
                     , line :: Int }
            deriving (Show, Eq)


-- | LexDat state constructor
mkLexDat :: Token -> Int -> LexDat
mkLexDat t n = LexDat t n

