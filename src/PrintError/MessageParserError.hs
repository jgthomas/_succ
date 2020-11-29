-- |
-- Module       : MessageParserError
-- Description  : Format parser error messages
--
-- Formats error messages of the parser error type
module PrintError.MessageParserError
  ( parserErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..))
import qualified PrintError.PrintErrorTokens as PrintErrorTokens
  ( buildLineMsg,
    buildTokMsg,
  )
import Types.Error (ParserError (..))
import Types.Tokens

-- | Generate parser error message
parserErrorMsg :: ParserError -> (String, PrintRange)
parserErrorMsg err@(TreeError _) = (show err, All)
parserErrorMsg (LexDataError []) = (msg, None)
  where
    msg = "Empty input from lexer"
parserErrorMsg (LexDataError [d]) = (msg, range)
  where
    msg =
      buildLineMsg d
        <> "Unexpected input "
        <> PrintErrorTokens.buildTokMsg d
    range = Exact (line . tokenData $ d)
parserErrorMsg (LexDataError (d : _)) = (msg, range)
  where
    msg =
      buildLineMsg d
        <> "Unexpected input starting at '"
        <> PrintErrorTokens.buildTokMsg d
        <> "'"
    range = From (line . tokenData $ d)

buildLineMsg :: Token -> String
buildLineMsg token = PrintErrorTokens.buildLineMsg . line . tokenData $ token
