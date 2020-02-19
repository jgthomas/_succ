{-|
Module       : LexTab
Description  : State data types for Lexer

Data types used for the Lexer stage of succ.
-}
module LexTab where


import Tokens (Token)


-- | LexTab state data type
data LexTab = LexTab { datList :: [LexDat]
                     , lineNum :: Int }

-- | LexTab state constructor
mkLexTab :: LexTab
mkLexTab = LexTab [] 1


-- | LexDat state data type
data LexDat = LexDat { tok  :: Token
                     , line :: Int }
            deriving (Show, Eq)


-- | LexDat state constructor
mkLexDat :: Token -> Int -> LexDat
mkLexDat t n = LexDat t n

