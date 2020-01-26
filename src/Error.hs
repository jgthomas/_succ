
module Error where


import AST      (Tree)
import LexDat   (LexDat)
import Operator (Operator)
import Tokens   (Keyword, OpTok, Token)
import Type     (Type)


data CompilerError = LexerError LexerError
                   | ParserError ParserError
                   | GeneratorError GeneratorError
                   | SyntaxError SyntaxError
                   | TypeError TypeError
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = EmptyInput
                | UnexpectedInput String
                deriving (Show, Eq)


data ParserError = TreeError Tree
                 | LexDataError [LexDat]
                 deriving (Show, Eq)


data GeneratorError = NoStateFound String
                    | UndefinedScope Int
                    | OperatorError Operator
                    deriving (Show, Eq)


data SyntaxError = InvalidIdentifier Token
                 | NonValidIdentifier LexDat
                 | MissingToken Token
                 | UnexpectedToken Token
                 | UnexpectedLexDat LexDat
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
               | BadType LexDat
               | TypeMismatch [Type] [Type]
               | MissingType String
               | UnexpectedType Type
               | NotTyped Tree
               deriving (Show, Eq)

