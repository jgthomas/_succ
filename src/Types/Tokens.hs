
module Types.Tokens where


data Token = OpenBracket OpenBracket LexDat
           | CloseBracket CloseBracket LexDat
           | OpTok OpTok LexDat
           | Ident String LexDat
           | ConstInt Int LexDat
           | Keyword Keyword LexDat
           | Separator Separator LexDat
           deriving (Show, Eq)


data SynTok = Open OpenBracket
            | Close CloseBracket
            | Word Keyword
            | Sep Separator
            deriving (Show, Eq)


data Separator = SemiColon
               | Comma
               | QuestMark
               | Colon
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
tokenData (Separator SemiColon dat) = dat
tokenData (Separator Colon dat)     = dat
tokenData (Separator QuestMark dat) = dat
tokenData (Separator Comma dat)     = dat
tokenData (OpenBracket _ dat)       = dat
tokenData (CloseBracket _ dat)      = dat
tokenData (Ident _ dat)             = dat
tokenData (ConstInt _ dat)          = dat
tokenData (Keyword _ dat)           = dat
tokenData (OpTok _ dat)             = dat
