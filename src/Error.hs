
module Error where


import AST    (Tree)
import Tokens (Keyword, OpTok, Token)
import Type   (Type)


data CompilerError = LexerError LexerError
                   | ParserError ParserError
                   | GeneratorError GeneratorError
                   | SyntaxError SyntaxError
                   | TypeError TypeError
                   | OperatorError Token
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = BadInput String
                | EmptyInput
                deriving (Show, Eq)


data ParserError = TreeError Tree
                 | TokensError [Token]
                 deriving (Show, Eq)


data GeneratorError = NoStateFound String
                    | UndefinedScope Int
                    deriving (Show, Eq)


data SyntaxError = InvalidIdentifier Token
                 | MissingToken Token
                 | UnexpectedToken Token
                 | MissingIdentifier
                 | MissingKeyword Keyword
                 | Undeclared Tree
                 | Undefined Tree
                 | DoubleDeclared Tree
                 | DoubleDefined Tree
                 | Unrecognised Tree
                 | Unexpected Tree
                 | MisMatch Int Tree
                 | InvalidCall Tree
                 | UnexpectedOp OpTok
                 deriving (Show, Eq)


data TypeError = InvalidType Token | Type
               | TypeMismatch [Type] [Type]
               | MissingType String
               | UnexpectedType Type
               | NotTyped Tree
               deriving (Show, Eq)

