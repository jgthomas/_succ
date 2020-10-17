module LexerTest.TestUtility
  ( dummyData,
    metaData,
  )
where

import Types.Tokens (LexDat (..), Token (..), dummyLexDat, tokenData)

dummyData :: [Token] -> [Token]
dummyData tokens = map setDummy tokens

setDummy :: Token -> Token
setDummy (Separator a _) = Separator a dummyLexDat
setDummy (OpenBracket a _) = OpenBracket a dummyLexDat
setDummy (CloseBracket a _) = CloseBracket a dummyLexDat
setDummy (Ident a _) = Ident a dummyLexDat
setDummy (ConstInt a _) = ConstInt a dummyLexDat
setDummy (Keyword a _) = Keyword a dummyLexDat
setDummy (OpTok a _) = OpTok a dummyLexDat

metaData :: Token -> (String, Int)
metaData token = (input . tokenData $ token, line . tokenData $ token)
