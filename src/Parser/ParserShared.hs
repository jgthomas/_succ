
module Parser.ParserShared
        (parseBracketedSeq,
         verifyAndConsume,
         consumeTok,
         consumeNToks,
         makeNodeDat
        ) where


import Control.Monad   (unless)

import Parser.ParState (ParserState, throwError)
import Types.AST       (NodeDat, Tree (..), mkNodeDat)
import Types.Error     (CompilerError (ParserError, SyntaxError),
                        ParserError (..), SyntaxError (..))
import Types.LexDat    (LexDat (..))
import Types.Tokens


parseBracketedSeq :: [Tree]
                  -> [LexDat]
                  -> ([Tree] -> [LexDat] -> ParserState ([Tree], [LexDat]))
                  -> ParserState ([Tree], [LexDat])
parseBracketedSeq _ [] _ = throwError $ ParserError (LexDataError [])
parseBracketedSeq xs lexData@(LexDat{tok=OpenBracket _}:LexDat{tok=CloseBracket _}:_) _ = do
        lexData' <- consumeTok lexData
        pure (reverse xs, lexData')
parseBracketedSeq xs lexData@(LexDat{tok=CloseBracket _}:_) _ =
        pure (reverse xs, lexData)
parseBracketedSeq _ (d@LexDat{tok=Comma}:LexDat{tok=CloseBracket _}:_) _ =
        throwError $ SyntaxError (UnexpectedLexDat d)
parseBracketedSeq xs (LexDat{tok=OpenBracket _}:rest) f =
        f xs rest
parseBracketedSeq xs (LexDat{tok=Comma}:rest) f =
        f xs rest
parseBracketedSeq _ (a:_) _ = throwError $ SyntaxError (UnexpectedLexDat a)


verifyAndConsume :: Token -> [LexDat] -> ParserState [LexDat]
verifyAndConsume t lexData = do
        nextTokIs t lexData
        consumeTok lexData


nextTokIs :: Token -> [LexDat] -> ParserState ()
nextTokIs _ []    = throwError $ ParserError (LexDataError [])
nextTokIs t [a]   = isTok t a
nextTokIs t (a:_) = isTok t a


isTok :: Token -> LexDat -> ParserState ()
isTok t a = unless (t == tok a) $ throwError $ SyntaxError (MissingToken t a)


consumeTok :: [LexDat] -> ParserState [LexDat]
consumeTok []          = throwError $ ParserError (LexDataError [])
consumeTok [_]         = pure []
consumeTok (_:lexData) = pure lexData


consumeNToks :: Int -> [LexDat] -> ParserState [LexDat]
consumeNToks 0 lexData = pure lexData
consumeNToks n lexData = do
        lexData' <- consumeTok lexData
        consumeNToks (pred n) lexData'


makeNodeDat :: [LexDat] -> ParserState NodeDat
makeNodeDat []    = throwError $ ParserError (LexDataError [])
makeNodeDat (d:_) = pure $ mkNodeDat (line d) (line d)
