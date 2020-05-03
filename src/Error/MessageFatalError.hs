{-|
Module       : MessageFatalError
Description  : Generate FatalError messages

Create error messages with for the FatalError error type
-}
module Error.MessageFatalError (fatalErrorMsg) where


import Error.Error            (FatalError (..))
import Error.PrintErrorTokens (PrintRange (..))


-- | Generate string for FatalError message
fatalErrorMsg :: FatalError -> (String, PrintRange)

fatalErrorMsg err@(LexerBug input) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ input ++ fatalErrorMsgOutro

fatalErrorMsg err@(ParserBug lexData) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show lexData ++ fatalErrorMsgOutro

fatalErrorMsg err@(CheckerBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro

fatalErrorMsg err@(GeneratorBug tree) = (msg, None)
        where msg = fatalErrorMsgIntro err ++ show tree ++ fatalErrorMsgOutro


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
             GeneratorBug{} -> "code generator"


fatalErrorMsgOutro :: String
fatalErrorMsgOutro = " good luck!"
