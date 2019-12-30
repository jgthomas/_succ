
module Tokens where


data Token = OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | SemiColon
           | OpTok OpTok
           | Ident String
           | ConstInt Int
           | Keyword Keyword
           | Colon
           | QuestMark
           | Comma
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
           deriving (Show, Eq)


data OpTokType = LogicalOR
               | LogicalAND
               | Factor
               | Term
               | Assign
               | Equality
               | Relational
               | PostPosition
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
                              PercentEqual]
             PostPosition -> [PlusPlus,
                              MinusMinus]
