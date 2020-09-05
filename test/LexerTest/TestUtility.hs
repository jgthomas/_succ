
module LexerTest.TestUtility (dummyData) where


import Types.Tokens


dummyData :: [Token] -> [Token]
dummyData tokens = map setDummy tokens


setDummy :: Token -> Token
setDummy (SemiColon _)      = SemiColon dummyLexDat
setDummy (Colon _)          = Colon dummyLexDat
setDummy (QuestMark _)      = QuestMark dummyLexDat
setDummy (Comma _)          = Comma dummyLexDat
setDummy (OpenBracket a _)  = OpenBracket a dummyLexDat
setDummy (CloseBracket a _) = CloseBracket a dummyLexDat
setDummy (Ident a _)        = Ident a dummyLexDat
setDummy (ConstInt a _)     = ConstInt a dummyLexDat
setDummy (Keyword a _)      = Keyword a dummyLexDat
setDummy (OpTok a _)        = OpTok a dummyLexDat
