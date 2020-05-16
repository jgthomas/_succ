
module TestUtility (makeLexDat, makeNodeDat) where


import Types.AST    (NodeDat (..))
import Types.LexDat (LexDat (LexDat))
import Types.Tokens (Token)


dummyLine :: Int
dummyLine = 0


makeLexDat :: Token -> LexDat
makeLexDat t = LexDat t dummyLine


makeNodeDat :: NodeDat
makeNodeDat = NodeDat dummyLine dummyLine
