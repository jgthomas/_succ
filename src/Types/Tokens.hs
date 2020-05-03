
module Types.Tokens where


import Data.Char (toLower)


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
           deriving (Eq)


data OpenBracket = OpenParen
                 | OpenBrace
                 | OpenSqBracket
                 deriving (Eq)


data CloseBracket = CloseParen
                  | CloseBrace
                  | CloseSqBracket
                  deriving (Eq)


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
           deriving (Eq)


instance Show Token where
        show SemiColon                     = ";"
        show Colon                         = ":"
        show QuestMark                     = "?"
        show Comma                         = ","
        show (OpenBracket OpenParen)       = "("
        show (OpenBracket OpenBrace)       = "{"
        show (OpenBracket OpenSqBracket)   = "["
        show (CloseBracket CloseBrace)     = "}"
        show (CloseBracket CloseParen)     = ")"
        show (CloseBracket CloseSqBracket) = "]"
        show (Ident a)                     = a
        show (ConstInt n)                  = show n
        show (Keyword kwd)                 = map toLower (show kwd)
        show (OpTok op)                    = showOpTok op


showOpTok :: OpTok -> String
showOpTok opTok =
        case opTok of
             PlusSign          -> "+"
             MinusSign         -> "-"
             Asterisk          -> "*"
             Backslash         -> "/"
             Percent           -> "%"
             Tilde             -> "~"
             Bang              -> "!"
             PipePipe          -> "||"
             AmpAmp            -> "&&"
             RightArrow        -> ">"
             RightArrowEqual   -> ">="
             LeftArrow         -> "<"
             LeftArrowEqual    -> "<="
             EqualEqual        -> "=="
             BangEqual         -> "!="
             EqualSign         -> "="
             PlusEqual         -> "+="
             MinusEqual        -> "-="
             AsteriskEqual     -> "*="
             BackslashEqual    -> "/="
             PercentEqual      -> "%="
             Ampersand         -> "&"
             PlusPlus          -> "++"
             MinusMinus        -> "--"
             Caret             -> "^"
             Pipe              -> "|"
             AmpEqual          -> "&="
             CaretEqual        -> "^="
             PipeEqual         -> "|="
             DoubleLeftArrow   -> "<<"
             DoubleRightArrow  -> ">>"
             DoubleLArrowEqual -> "<<="
             DoubleRArrowEqual -> ">>="


data OpTokType = LogicalOR
               | LogicalAND
               | Factor
               | Term
               | Assign
               | Equality
               | Relational
               | PostPosition
               | BitwiseXOR
               | BitwiseAND
               | BitwiseOR
               | Shift
               deriving (Eq)


isAssign :: OpTok -> Bool
isAssign op = op `elem` kind Assign


isPostPos :: OpTok -> Bool
isPostPos op = op `elem` kind PostPosition


kind :: OpTokType -> [OpTok]
kind tokTyp =
        case tokTyp of
             LogicalOR    -> [PipePipe]
             LogicalAND   -> [AmpAmp]
             Factor       -> [Asterisk,
                              Backslash,
                              Percent]
             Term         -> [PlusSign,
                              MinusSign]
             Equality     -> [EqualEqual,
                              BangEqual]
             Relational   -> [RightArrow,
                              LeftArrow,
                              RightArrowEqual,
                              LeftArrowEqual]
             Assign       -> [EqualSign,
                              PlusEqual,
                              MinusEqual,
                              AsteriskEqual,
                              BackslashEqual,
                              PercentEqual,
                              AmpEqual,
                              CaretEqual,
                              PipeEqual,
                              DoubleLArrowEqual,
                              DoubleRArrowEqual]
             PostPosition -> [PlusPlus,
                              MinusMinus]
             BitwiseXOR   -> [Caret]
             BitwiseAND   -> [Ampersand]
             BitwiseOR    -> [Pipe]
             Shift        -> [DoubleLeftArrow,
                              DoubleRightArrow]
