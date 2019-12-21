
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
           | Wut
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
           | BackSlash
           | Percent
           | Tilde
           | Bang
           | PipePipe
           | AmpAmp
           | RightArrow
           | RightArrowEquals
           | LeftArrow
           | LeftArrowEquals
           | EqualEqual
           | BangEqual
           | EqualSign
           | PlusEqual
           | MinusEqual
           | AsteriskEqual
           | BackSlashEqual
           | PercentEqual
           | Ampersand
           deriving (Show, Eq)


data TokenType = Unary
               | LogicalOR
               | LogicalAND
               | Factor
               | Term
               | Assign
               | Equality
               | Relational
               deriving (Eq)


isUnary :: OpTok -> Bool
isUnary op = op `elem` kind Unary


isAssign :: OpTok -> Bool
isAssign op = op `elem` kind Assign


kind :: TokenType -> [OpTok]
kind tokTyp =
        case tokTyp of
             Unary      -> [MinusSign,
                            Tilde,
                            Bang]
             LogicalOR  -> [PipePipe]
             LogicalAND -> [AmpAmp]
             Factor     -> [Asterisk,
                            BackSlash,
                            Percent]
             Term       -> [PlusSign,
                            MinusSign]
             Equality   -> [EqualEqual,
                            BangEqual]
             Relational -> [RightArrow,
                            LeftArrow,
                            RightArrowEquals,
                            LeftArrowEquals]
             Assign     -> [EqualSign,
                            PlusEqual,
                            MinusEqual,
                            AsteriskEqual,
                            BackSlashEqual,
                            PercentEqual]
