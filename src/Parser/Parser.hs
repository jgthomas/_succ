{-|
Module       : Parser
Description  : Produces an abstract syntax tree

Converts a list of tokens into an abstract syntax tree
representing the C program.
-}
module Parser.Parser (parse) where


import           Lexer.LexTab             (LexDat (..))
import           Parser.ParserDeclaration (parseDeclaration)
import           Parser.ParserFunction    (parseFunction)
import           Parser.ParState          (ParserState, runParState, throwError)
import qualified Parser.ParState          as ParState (getState, putState,
                                                       startState)
import           Types.AST                (Tree (..))
import           Types.Error              (CompilerError (ParserError, SyntaxError),
                                           ParserError (..), SyntaxError (..))
import           Types.Tokens             (OpTok (..), OpenBracket (..),
                                           Token (..))


-- | Convert a list of tokens into an AST
parse :: [LexDat] -> Either CompilerError Tree
parse lexData = runParState parseTokens lexData ParState.startState


parseTokens :: [LexDat] -> ParserState Tree
parseTokens []      = throwError $ ParserError (LexDataError [])
parseTokens lexData = parseTopLevelItems lexData


parseTopLevelItems :: [LexDat] -> ParserState Tree
parseTopLevelItems [] = ProgramNode . reverse <$> ParState.getState
parseTopLevelItems lexData@(LexDat{tok=Keyword _}:_) = do
        items            <- ParState.getState
        (item, lexData') <- parseTopLevelItem lexData
        ParState.putState $ ProgramNode (item:items)
        parseTopLevelItems lexData'
parseTopLevelItems lexData = throwError $ ParserError (LexDataError lexData)


parseTopLevelItem :: [LexDat] -> ParserState (Tree, [LexDat])
parseTopLevelItem lexData@(_:_:_:LexDat{tok=OpenBracket OpenParen}:_)  = parseFunction lexData
parseTopLevelItem lexData@(_:_:LexDat{tok=OpenBracket OpenParen}:_)    = parseFunction lexData
parseTopLevelItem lexData@(_:LexDat{tok=Ident _}:_)        = parseDeclaration lexData
parseTopLevelItem lexData@(_:LexDat{tok=OpTok Asterisk}:_) = parseDeclaration lexData
parseTopLevelItem (_:b:_) = throwError $ SyntaxError (NonValidIdentifier b)
parseTopLevelItem lexData = throwError $ ParserError (LexDataError lexData)
