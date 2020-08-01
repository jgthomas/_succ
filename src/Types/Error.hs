
module Types.Error where


import Types.AssemblySchema (AssemblySchema)
import Types.AST            (Tree)
import Types.LexDat         (LexDat)
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
                 | LexDataError [LexDat]
                 deriving (Show, Eq)


data SyntaxError = NonValidIdentifier LexDat
                 | MissingToken Token LexDat
                 | UnexpectedLexDat LexDat
                 | MissingKeyword Keyword LexDat
                 | LengthMismatch Tree Int Int
                 | UndeclaredLen Tree
                 | BadType LexDat
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
                | ParserBug [LexDat]
                | CheckerBug Tree
                | GeneratorBug Tree
                | BuilderBug AssemblySchema
                deriving (Show, Eq)
