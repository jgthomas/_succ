{-|
Module       : TokToNodeData
Description  : Builds tree node metadata

Builds tree node metadata from metadata captured by the lexer.
-}
module Parser.TokToNodeData (makeNodeDat) where


import Parser.ParState (ParserState, throwError)
import Types.AST       (NodeDat (NodeDat))
import Types.Error     (CompilerError (ParserError), ParserError (..))
import Types.Tokens


-- | Make metadata for node in an AST
makeNodeDat :: [Token] -> ParserState NodeDat
makeNodeDat []        = throwError $ ParserError (LexDataError [])
makeNodeDat (token:_) = pure . lexDatToNodeDat . tokenData $ token


lexDatToNodeDat :: LexDat -> NodeDat
lexDatToNodeDat lexDat = NodeDat (line lexDat) (line lexDat) False False 1
