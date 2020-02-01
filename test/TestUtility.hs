
module TestUtility where


import           AST    (NodeDat)
import qualified AST    (mkNodeDat)
import           LexDat (LexDat (LexDat))
import           Tokens (Token)


dummyLine :: Int
dummyLine = 0


mkLexDat :: Token -> LexDat
mkLexDat t = LexDat t dummyLine


mkNodeDat :: NodeDat
mkNodeDat = AST.mkNodeDat dummyLine dummyLine

