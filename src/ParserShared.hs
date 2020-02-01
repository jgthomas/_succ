
module ParserShared
        (parsePassIn,
         verifyAndConsume,
         consumeTok,
         consumeNToks,
         parseType,
         mkDat,
         nextTokIsNot
        ) where


import Control.Monad (unless)

import AST           (NodeDat, Tree (..), mkNodeDat)
import Error         (CompilerError (ParserError, SyntaxError),
                      ParserError (..), SyntaxError (..))
import LexDat        (LexDat (..))
import ParState      (ParserState, throwError)
import Tokens        (Keyword (..), OpTok (..), Token (..))
import Type          (Type (..))


parsePassIn :: [Tree]
            -> [LexDat]
            -> ([Tree] -> [LexDat] -> ParserState ([Tree], [LexDat]))
            -> ParserState ([Tree], [LexDat])
parsePassIn _ [] _ = throwError $ ParserError (LexDataError [])
parsePassIn xs (LexDat{tok=OpenParen}:LexDat{tok=CloseParen}:rest) _ =
        pure (xs, rest)
parsePassIn xs (LexDat{tok=CloseParen}:rest) _ = pure (reverse xs, rest)
parsePassIn _ (d@LexDat{tok=Comma}:LexDat{tok=CloseParen}:_) _ =
        throwError $ SyntaxError (UnexpectedLexDat d)
parsePassIn xs (LexDat{tok=OpenParen}:rest) f = f xs rest
parsePassIn xs (LexDat{tok=Comma}:rest) f     = f xs rest
parsePassIn _ (a:_) _ = throwError $ SyntaxError (UnexpectedLexDat a)


verifyAndConsume :: Token -> [LexDat] -> ParserState [LexDat]
verifyAndConsume t lexData = do
        nextTokIs t lexData
        consumeTok lexData


nextTokIs :: Token -> [LexDat] -> ParserState ()
nextTokIs _ []    = throwError $ ParserError (LexDataError [])
nextTokIs t [a]   = isTok t a
nextTokIs t (a:_) = isTok t a


nextTokIsNot :: Token -> [LexDat] -> ParserState ()
nextTokIsNot _ []    = throwError $ ParserError (LexDataError [])
nextTokIsNot t [a]   = isNotTok t a
nextTokIsNot t (a:_) = isNotTok t a


isTok :: Token -> LexDat -> ParserState ()
isTok t a = unless (t == tok a) $ throwError $ SyntaxError (MissingToken t a)


isNotTok :: Token -> LexDat -> ParserState ()
isNotTok t a = unless ( t /= tok a) $ throwError $ SyntaxError (UnexpectedLexDat a)


consumeTok :: [LexDat] -> ParserState [LexDat]
consumeTok []          = throwError $ ParserError (LexDataError [])
consumeTok [_]         = pure []
consumeTok (_:lexData) = pure lexData


consumeNToks :: Int -> [LexDat] -> ParserState [LexDat]
consumeNToks 0 lexData = pure lexData
consumeNToks n lexData = do
        lexData' <- consumeTok lexData
        consumeNToks (pred n) lexData'


parseType :: [LexDat] -> ParserState Type
parseType (LexDat{tok=Keyword Int}:LexDat{tok=OpTok Asterisk}:_) =
        pure IntPointer
parseType (LexDat{tok=Keyword Int}:_) = pure IntVar
parseType (a:_) = throwError $ SyntaxError (BadType a)
parseType lexData  = throwError $ ParserError (LexDataError lexData)


mkDat :: LexDat -> ParserState NodeDat
mkDat d = pure $ mkNodeDat (line d) (line d)
