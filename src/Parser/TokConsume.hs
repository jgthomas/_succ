{-|
Module       : TokConsume
Description  : Verifies and consumes lexed tokens

Verifies and consumes tokens created by the lexer.
-}
module Parser.TokConsume (verifyAndConsume, consumeNToks, consumeTok) where


import Control.Monad   (unless)

import Parser.ParState (ParserState, throwError)
import Types.Error     (CompilerError (ParserError, SyntaxError),
                        ParserError (..), SyntaxError (..))
import Types.LexDat    (LexDat (..))
import Types.Tokens    (Token)


-- | Verifies the type of a token, then consumes that token
verifyAndConsume :: Token -> [LexDat] -> ParserState [LexDat]
verifyAndConsume t lexData = do
        nextTokIs t lexData
        consumeTok lexData


-- | Consumes the next token in the list
consumeTok :: [LexDat] -> ParserState [LexDat]
consumeTok []          = throwError $ ParserError (LexDataError [])
consumeTok [_]         = pure []
consumeTok (_:lexData) = pure lexData


-- | Consumes the next N tokens in the list
consumeNToks :: Int -> [LexDat] -> ParserState [LexDat]
consumeNToks 0 lexData = pure lexData
consumeNToks n lexData = do
        lexData' <- consumeTok lexData
        consumeNToks (pred n) lexData'


nextTokIs :: Token -> [LexDat] -> ParserState ()
nextTokIs _ []    = throwError $ ParserError (LexDataError [])
nextTokIs t [a]   = isTok t a
nextTokIs t (a:_) = isTok t a


isTok :: Token -> LexDat -> ParserState ()
isTok t a = unless (t == tok a) $ throwError $ SyntaxError (MissingToken t a)
