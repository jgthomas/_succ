{-|
Module       : MessageFatalError
Description  : Generate FatalError messages

Create error messages with for the FatalError error type
-}
module PrintError.MessageFatalError (fatalErrorMsg) where


import PrintError.PrintErrorTokens (PrintRange (..))
import Types.Error                 (FatalError (..))


-- | Generate string for FatalError message
fatalErrorMsg :: FatalError -> (String, PrintRange)

fatalErrorMsg err@(LexerBug input) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ input ++ fatalErrorMsgOutro

fatalErrorMsg err@(ParserBug lexData) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show lexData ++ fatalErrorMsgOutro

fatalErrorMsg err@(CheckerBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro

fatalErrorMsg err@(ConverterBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro

fatalErrorMsg err@(BuilderBug schema) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show schema ++ fatalErrorMsgOutro


fatalErrorMsgIntro :: FatalError -> String
fatalErrorMsgIntro err = "There is a bug in the " ++ component
                         ++ ", this is the state I was working with: "
        where component = fatalComponent err


fatalComponent :: FatalError -> String
fatalComponent err =
        case err of
             LexerBug{}     -> "lexer"
             ParserBug{}    -> "parser"
             CheckerBug{}   -> "syntax tree checker"
             ConverterBug{} -> "syntax tree to assembly schema converter"
             BuilderBug{}   -> "assembly generator"


fatalErrorMsgOutro :: String
fatalErrorMsgOutro = " good luck!"
