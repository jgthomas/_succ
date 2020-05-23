
module Types.Tokens where


data Token = OpenBracket OpenBracket
           | CloseBracket CloseBracket
           | SemiColon
           | OpTok OpTok
           | Ident String
           | ConstInt Int
           | Keyword Keyword
           | Colon
           | QuestMark
           | Comma
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
