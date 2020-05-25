
module PrintError.MessageStageError
        (lexerErrorMsg,
         parserErrorMsg
        ) where


import PrintError.PrintErrorTokens (PrintRange (..), buildLineMsg, buildTokMsg)
import Types.Error                 (LexerError (..), ParserError (..))
import Types.LexDat                (LexDat (..))


lexerErrorMsg :: LexerError -> (String, PrintRange)

lexerErrorMsg (UnexpectedInput s) = (msg, All)
        where msg = lexerUnexpectedMsg s

lexerErrorMsg EmptyInput = (msg, None)
        where msg = "Empty input file"


lexerUnexpectedMsg :: String -> String
lexerUnexpectedMsg str =
        case str of
             []    -> msg ++ "Empty file"
             [c]   -> msg ++ "'" ++ [c] ++ "'"
             (c:_) -> msg ++ "'" ++ [c] ++ "'"
        where msg = "Unexpected input: "


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
