
module ParserTest.TestUtility where


import TestUtility  (makeLexDat)
import Types.LexDat
import Types.Tokens


makeLexData :: [Token] -> [LexDat]
makeLexData toks = map makeLexDat toks
