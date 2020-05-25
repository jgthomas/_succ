{-|
Module       : MessageParserError
Description  : Format parser error messages

Formats error messages of the parser error type
-}
module PrintError.MessageParserError (parserErrorMsg) where


import PrintError.PrintErrorTokens (PrintRange (..), buildLineMsg, buildTokMsg)
import Types.Error                 (ParserError (..))
import Types.LexDat                (LexDat (..))


-- | Generate parser error message
parserErrorMsg :: ParserError -> (String, PrintRange)

parserErrorMsg err@(TreeError _) = (show err, All)
parserErrorMsg (LexDataError []) = (msg, None)
        where msg = "Empty input from lexer"

parserErrorMsg (LexDataError [d])  = (msg, Exact $ line d)
        where msg = buildLineMsg (line d)
                    ++ "Unexpected input "
                    ++ buildTokMsg (tok d)

parserErrorMsg (LexDataError (d:_)) = (msg, From $ line d)
        where msg = buildLineMsg (line d)
                    ++ "Unexpected input starting at '"
                    ++ buildTokMsg (tok d) ++ "'"
