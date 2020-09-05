
module Types.Error where


import Types.AssemblySchema (AssemblySchema)
import Types.AST            (Tree)
import Types.Operator       (Operator)
import Types.Tokens         (Keyword, Token)
import Types.Type           (Type)


data CompilerError = LexerError LexerError
                   | ParserError ParserError
                   | SyntaxError SyntaxError
                   | StateError StateError
                   | ScopeError ScopeError
                   | TypeError TypeError
                   | LogicError LogicError
                   | FatalError FatalError
                   | ImpossibleError
                   deriving (Show, Eq)


data LexerError = EmptyInput
                | UnexpectedInput String
                deriving (Show, Eq)


data ParserError = TreeError Tree
                 | LexDataError [Token]
                 deriving (Show, Eq)


data SyntaxError = NonValidIdentifier Token
                 | MissingToken Token Token
                 | UnexpectedLexDat Token
                 | MissingKeyword Keyword Token
                 | LengthMismatch Tree Int Int
                 | UndeclaredLen Tree
                 | BadType Token
                 deriving (Show, Eq)


data StateError = NoStateFound String
                | UndefinedScope String Int
                deriving (Show, Eq)


data ScopeError = UndeclaredNode Tree
                | UndefinedNode Tree
                | DoubleDeclaredNode Tree
                | DoubleDefinedNode Tree
                | UnrecognisedNode Tree
                | UnexpectedNode Tree
                | MisMatchNode Int Tree
                | InvalidCallNode Tree
                | Unaddressable Tree
                deriving (Show, Eq)


data TypeError = TypeMismatch [Type] [Type] Tree
               | MissingType Tree
               | UnexpectedType Type Tree
               | NotTyped Tree
               | TypeCheckingImpossible Tree
               deriving (Show, Eq)


data LogicError = OperatorUseError Tree Operator
                | MalformedNode Tree
                | AssignmentLogicError Tree Operator
                | AssignmentTreeError Tree
                deriving (Show, Eq)


data FatalError = LexerBug String
                | ParserBug [Token]
                | ConverterBug Tree
                | BuilderBug AssemblySchema
                deriving (Show, Eq)
