
module TestUtility where


import           Lexer.LexTab (LexDat (LexDat))
import           Types.AST    (NodeDat)
import qualified Types.AST    as AST (mkNodeDat)
import           Types.Tokens (Token)


dummyLine :: Int
dummyLine = 0


mkLexDat :: Token -> LexDat
mkLexDat t = LexDat t dummyLine


mkNodeDat :: NodeDat
mkNodeDat = AST.mkNodeDat dummyLine dummyLine

