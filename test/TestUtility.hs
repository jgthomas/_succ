
module TestUtility where


import           Types.AST    (NodeDat)
import qualified Types.AST    as AST (mkNodeDat)
import           Types.LexDat (LexDat (LexDat))
import           Types.Tokens (Token)


dummyLine :: Int
dummyLine = 0


mkLexDat :: Token -> LexDat
mkLexDat t = LexDat t dummyLine


mkNodeDat :: NodeDat
mkNodeDat = AST.mkNodeDat dummyLine dummyLine

