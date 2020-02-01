{-|
Module       : Parser
Description  : Produces an abstract syntax tree

Converts a list of tokens into an abstract syntax tree
representing the C program.
-}
module Parser (parse) where


import           AST               (Tree (..))
import           Error             (CompilerError (ImpossibleError, ParserError, SyntaxError),
                                    ParserError (..), SyntaxError (..))
import           LexDat            (LexDat (..))
import           ParState          (ParserState, runParState, throwError)
import qualified ParState          (getState, putState, startState)
import           Tokens            (OpTok (..), Token (..))


import           ParserDeclaration (parsePointerDec, parseValueDec)
import           ParserFunction    (parseFunction)


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
parseTopLevelItem lexData@(_:_:_:LexDat{tok=OpenParen}:_)  = parseFunction lexData
parseTopLevelItem lexData@(_:_:LexDat{tok=OpenParen}:_)    = parseFunction lexData
parseTopLevelItem lexData@(_:LexDat{tok=Ident _}:_)        = parseValueDec lexData
parseTopLevelItem lexData@(_:LexDat{tok=OpTok Asterisk}:_) = parsePointerDec lexData
parseTopLevelItem []      = throwError ImpossibleError
parseTopLevelItem (_:b:_) = throwError $ SyntaxError (NonValidIdentifier b)
parseTopLevelItem lexData = throwError $ ParserError (LexDataError lexData)
