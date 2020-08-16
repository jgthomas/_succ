{-|
Module       : TokToNodeData
Description  : Builds tree node metadata

Builds tree node metadata from metadata captured by the lexer.
-}
module Parser.TokToNodeData (makeNodeDat) where


import Parser.ParState (ParserState, throwError)
import Types.AST       (NodeDat (NodeDat))
import Types.Error     (CompilerError (ParserError), ParserError (..))
import Types.LexDat    (LexDat (line))


-- | Make metadata for node in an AST
makeNodeDat :: [LexDat] -> ParserState NodeDat
makeNodeDat []    = throwError $ ParserError (LexDataError [])
makeNodeDat (d:_) = pure . lexDatToNodeDat $ d


lexDatToNodeDat :: LexDat -> NodeDat
lexDatToNodeDat lexDat = NodeDat (line lexDat) (line lexDat) False 1
