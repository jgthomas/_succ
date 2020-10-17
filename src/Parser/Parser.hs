-- |
-- Module       : Parser
-- Description  : Produces an abstract syntax tree
--
-- Converts a list of tokens into an abstract syntax tree
-- representing the C program.
module Parser.Parser
  ( parse,
  )
where

import Parser.ParState (ParserState, throwError)
import qualified Parser.ParState as ParState
  ( evaluate,
    getState,
    putState,
    startState,
  )
import Parser.ParserDeclaration (parseDeclaration)
import Parser.ParserFunction (parseFunction)
import Types.AST (Tree (..))
import Types.Error
  ( CompilerError (ParserError, SyntaxError),
    ParserError (..),
    SyntaxError (..),
  )
import Types.Tokens
  ( OpTok (..),
    OpenBracket (..),
    Token (..),
  )

-- | Convert a list of tokens into an AST
parse :: [Token] -> Either CompilerError Tree
parse tokens = ParState.evaluate parseTokens tokens ParState.startState

parseTokens :: [Token] -> ParserState Tree
parseTokens [] = throwError $ ParserError (LexDataError [])
parseTokens tokens = parseTopLevelItems tokens

parseTopLevelItems :: [Token] -> ParserState Tree
parseTopLevelItems [] = ProgramNode . reverse <$> ParState.getState
parseTopLevelItems tokens@(Keyword _ _ : _) = do
  items <- ParState.getState
  (item, tokens') <- parseTopLevelItem tokens
  ParState.putState $ item : items
  parseTopLevelItems tokens'
parseTopLevelItems tokens = throwError $ ParserError (LexDataError tokens)

parseTopLevelItem :: [Token] -> ParserState (Tree, [Token])
parseTopLevelItem tokens@(_ : _ : _ : OpenBracket OpenParen _ : _) = parseFunction tokens
parseTopLevelItem tokens@(_ : _ : OpenBracket OpenParen _ : _) = parseFunction tokens
parseTopLevelItem tokens@(_ : Ident _ _ : _) = parseDeclaration tokens
parseTopLevelItem tokens@(_ : OpTok Asterisk _ : _) = parseDeclaration tokens
parseTopLevelItem (_ : token : _) = throwError $ SyntaxError (NonValidIdentifier token)
parseTopLevelItem tokens = throwError $ ParserError (LexDataError tokens)
