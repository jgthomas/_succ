-- |
-- Module       : TokConsume
-- Description  : Verifies and consumes lexed tokens
--
-- Verifies and consumes tokens created by the lexer.
module Parser.TokConsume
  ( consume,
    checkAndConsume,
  )
where

import Control.Monad (unless)
import Parser.ParState (ParserState, throwError)
import Types.Error
  ( CompilerError (ImpossibleError, ParserError, SyntaxError),
    ParserError (..),
    SyntaxError (..),
  )
import Types.Tokens (SynTok (..), Token (..), tokenData)

-- | Consumes the next n tokens in the list
consume :: Int -> [Token] -> ParserState [Token]
consume n tokens = consumeNToks n tokens

-- | Checks the type of a token, then consumes token if type is correct
checkAndConsume :: SynTok -> [Token] -> ParserState [Token]
checkAndConsume _ [] = throwError $ ParserError (LexDataError [])
checkAndConsume synTok tokens = do
  token <- synTokToToken synTok $ head tokens
  verifyAndConsume token tokens

synTokToToken :: SynTok -> Token -> ParserState Token
synTokToToken (Open open) token = pure $ OpenBracket open $ tokenData token
synTokToToken (Close close) token = pure $ CloseBracket close $ tokenData token
synTokToToken (Word word) token = pure $ Keyword word $ tokenData token
synTokToToken (Sep sep) token = pure $ Separator sep $ tokenData token

verifyAndConsume :: Token -> [Token] -> ParserState [Token]
verifyAndConsume t tokens = do
  nextTokIs t tokens
  consumeNToks 1 tokens

consumeNToks :: Int -> [Token] -> ParserState [Token]
consumeNToks _ [] = throwError $ ParserError (LexDataError [])
consumeNToks n tokens
  | n < 1 = throwError ImpossibleError
  | n > length tokens = throwError $ ParserError (LexDataError tokens)
  | otherwise = pure . drop n $ tokens

nextTokIs :: Token -> [Token] -> ParserState ()
nextTokIs _ [] = throwError $ ParserError (LexDataError [])
nextTokIs t [a] = isTok t a
nextTokIs t (a : _) = isTok t a

isTok :: Token -> Token -> ParserState ()
isTok t1 t2 = unless (t1 == t2) $ throwError $ SyntaxError (MissingToken t1 t2)
