
module LexerTest.TestUtility (dummyData) where


import Types.Tokens


dummyData :: [Token] -> [Token]
dummyData tokens = map setDummy tokens


setDummy :: Token -> Token
setDummy (Separator SemiColon _) = Separator SemiColon dummyLexDat
setDummy (Separator Colon _)     = Separator Colon dummyLexDat
setDummy (Separator QuestMark _) = Separator QuestMark dummyLexDat
setDummy (Separator Comma _)     = Separator Comma dummyLexDat
setDummy (OpenBracket a _)       = OpenBracket a dummyLexDat
setDummy (CloseBracket a _)      = CloseBracket a dummyLexDat
setDummy (Ident a _)             = Ident a dummyLexDat
setDummy (ConstInt a _)          = ConstInt a dummyLexDat
setDummy (Keyword a _)           = Keyword a dummyLexDat
setDummy (OpTok a _)             = OpTok a dummyLexDat
