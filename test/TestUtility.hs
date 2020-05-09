
module TestUtility where


import           Types.AST    (NodeDat)
import qualified Types.AST    as AST (mkNodeDat)
import           Types.LexDat (LexDat (LexDat))
import           Types.Tokens (Token)


dummyLine :: Int
dummyLine = 0


makeLexDat :: Token -> LexDat
makeLexDat t = LexDat t dummyLine


makeLexData :: [Token] -> [LexDat]
makeLexData toks = map makeLexDat toks


makeNodeDat :: NodeDat
makeNodeDat = AST.mkNodeDat dummyLine dummyLine
