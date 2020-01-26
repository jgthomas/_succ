
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
                   | ScopeError ScopeError
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
                 | MissingToken Token LexDat
                 | UnexpectedToken Token
                 | UnexpectedLexDat LexDat
                 | MissingIdentifier
                 | MissingKeyword Keyword
                 | UnexpectedOp OpTok
                 deriving (Show, Eq)


data ScopeError = UndeclaredNode Tree
                | UndefinedNode Tree
                | DoubleDeclaredNode Tree
                | DoubleDefinedNode Tree
                | UnrecognisedNode Tree
                | UnexpectedNode Tree
                | MisMatchNode Int Tree
                | InvalidCallNode Tree
                deriving (Show, Eq)


data TypeError = InvalidType Token | Type
               | BadType LexDat
               | TypeMismatch [Type] [Type]
               | MissingType String
               | UnexpectedType Type
               | NotTyped Tree
               deriving (Show, Eq)

