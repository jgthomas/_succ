{-|
Module       : LexTab
Description  : State data types for Lexer

Data types used for the Lexer stage of succ.
-}
module Lexer.LexTab where


import Types.LexDat (LexDat)


-- | LexTab state data type
data LexTab = LexTab { datList :: [LexDat]
                     , lineNum :: Int }


-- | LexTab state constructor
mkLexTab :: LexTab
mkLexTab = LexTab [] 1
