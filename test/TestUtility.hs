
module TestUtility (makeLexDat, mockNodeDat) where


import Types.AST    (NodeDat (..))
import Types.LexDat (LexDat (LexDat))
import Types.Tokens (Token)


dummyLine :: Int
dummyLine = 0


makeLexDat :: Token -> LexDat
makeLexDat t = LexDat t dummyLine


mockNodeDat :: NodeDat
mockNodeDat = NodeDat dummyLine dummyLine
