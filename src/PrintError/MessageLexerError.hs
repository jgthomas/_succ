-- |
-- Module       : MessageLexerError
-- Description  : Format lexer error messages
--
-- Formats error messages of the lexer error type.
module PrintError.MessageLexerError
  ( lexerErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..))
import Types.Error (LexerError (..))

-- | Generate lexer error message
lexerErrorMsg :: LexerError -> (String, PrintRange)
lexerErrorMsg (UnexpectedInput s) = (msg, All)
  where
    msg = lexerUnexpectedMsg s
lexerErrorMsg EmptyInput = (msg, None)
  where
    msg = "Empty input file"

lexerUnexpectedMsg :: String -> String
lexerUnexpectedMsg str =
  case str of
    [] -> msg ++ "Empty file"
    [c] -> msg ++ "'" ++ [c] ++ "'"
    (c : _) -> msg ++ "'" ++ [c] ++ "'"
  where
    msg = "Unexpected input: "
