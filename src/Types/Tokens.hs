
module Types.Tokens where


data Token = OpenBracket OpenBracket LexDat
           | CloseBracket CloseBracket LexDat
           | SemiColon LexDat
           | OpTok OpTok LexDat
           | Ident String LexDat
           | ConstInt Int LexDat
           | Keyword Keyword LexDat
           | Colon LexDat
           | QuestMark LexDat
           | Comma LexDat
           deriving (Show, Eq)


data OpenBracket = OpenParen
                 | OpenBrace
                 | OpenSqBracket
                 deriving (Show, Eq)


data CloseBracket = CloseParen
                  | CloseBrace
                  | CloseSqBracket
                  deriving (Show, Eq)


data Keyword = Int
             | Return
             | If
             | Else
             | For
             | While
             | Do
             | Break
             | Continue
             deriving (Show, Eq)


data OpTok = PlusSign
           | MinusSign
           | Asterisk
           | Backslash
           | Percent
           | Tilde
           | Bang
           | PipePipe
           | AmpAmp
           | RightArrow
           | RightArrowEqual
           | LeftArrow
           | LeftArrowEqual
           | EqualEqual
           | BangEqual
           | EqualSign
           | PlusEqual
           | MinusEqual
           | AsteriskEqual
           | BackslashEqual
           | PercentEqual
           | Ampersand
           | PlusPlus
           | MinusMinus
           | Caret
           | Pipe
           | AmpEqual
           | CaretEqual
           | PipeEqual
           | DoubleLeftArrow
           | DoubleRightArrow
           | DoubleLArrowEqual
           | DoubleRArrowEqual
           deriving (Show, Eq)


data LexDat = LexDat { input :: String
                     , line  :: Int }
            deriving (Show, Eq)


dummyLexDat :: LexDat
dummyLexDat = LexDat "" 0

mkLexDat :: String -> Int -> LexDat
mkLexDat cs n = LexDat cs n


tokenData :: Token -> LexDat
tokenData (SemiColon dat)      = dat
tokenData (Colon dat)          = dat
tokenData (QuestMark dat)      = dat
tokenData (Comma dat)          = dat
tokenData (OpenBracket _ dat)  = dat
tokenData (CloseBracket _ dat) = dat
tokenData (Ident _ dat)        = dat
tokenData (ConstInt _ dat)     = dat
tokenData (Keyword _ dat)      = dat
tokenData (OpTok _ dat)        = dat


headTokenData :: [Token] -> LexDat
headTokenData []      = dummyLexDat
headTokenData [tok]   = tokenData tok
headTokenData (tok:_) = tokenData tok
