{-|
Module       : TokConsume
Description  : Verifies and consumes lexed tokens

Verifies and consumes tokens created by the lexer.
-}
module Parser.TokConsume (consumeNToks, consumeTok, checkAndConsume) where


import Control.Monad   (unless)

import Parser.ParState (ParserState, throwError)
import Types.Error     (CompilerError (ImpossibleError, ParserError, SyntaxError),
                        ParserError (..), SyntaxError (..))
import Types.Tokens


-- | Checks the type of a token, then consumes that token if as expected
checkAndConsume :: SynTok -> [Token] -> ParserState [Token]
checkAndConsume _ []    = throwError $ ParserError (LexDataError [])
checkAndConsume synTok tokens =
        let token = synTokToToken synTok $ head tokens
            in
        verifyAndConsume token tokens


synTokToToken :: SynTok -> Token -> Token
synTokToToken (Open open) token   = OpenBracket open $ tokenData token
synTokToToken (Close close) token = CloseBracket close $ tokenData token
synTokToToken (Word word) token   = Keyword word $ tokenData token
synTokToToken (Sep sep) token     = Separator sep $ tokenData token


verifyAndConsume :: Token -> [Token] -> ParserState [Token]
verifyAndConsume t tokens = do
        nextTokIs t tokens
        consumeTok tokens


-- | Consumes the next token in the list
consumeTok :: [Token] -> ParserState [Token]
consumeTok tokens = consumeNToks 1 tokens


-- | Consumes the next N tokens in the list
consumeNToks :: Int -> [Token] -> ParserState [Token]
consumeNToks _ [] = throwError $ ParserError (LexDataError [])
consumeNToks n tokens
        | n < 1             = throwError ImpossibleError
        | n > length tokens = throwError $ ParserError (LexDataError tokens)
        | otherwise         = pure . drop n $ tokens


nextTokIs :: Token -> [Token] -> ParserState ()
nextTokIs _ []    = throwError $ ParserError (LexDataError [])
nextTokIs t [a]   = isTok t a
nextTokIs t (a:_) = isTok t a


isTok :: Token -> Token -> ParserState ()
isTok t1 t2 = unless (t1 == t2) $ throwError $ SyntaxError (MissingToken t1 t2)
