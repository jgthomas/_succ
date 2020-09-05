{-|
Module       : PrintErrorTokens
Description  : Format tokens for printing

Formats tokens for pretty printing in error messages.
-}
module PrintError.PrintErrorTokens
        (PrintRange(..),
         buildLineMsg,
         buildTokMsg
        ) where


import Data.Char    (toLower)

import Types.Tokens


-- | Defines source code line ranges to print
data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                | From Int
                | Until Int
                deriving (Eq)


-- | Builds a message about the line where an error occurred
buildLineMsg :: Int -> String
buildLineMsg n = "Line " ++ show n ++ ": "


--tokenData :: Token -> LexDat
--tokenData (SemiColon dat)      = dat
--tokenData (Colon dat)          = dat
--tokenData (QuestMark dat)      = dat
--tokenData (Comma dat)          = dat
--tokenData (OpenBracket _ dat)  = dat
--tokenData (CloseBracket _ dat) = dat
--tokenData (Ident _ dat)        = dat
--tokenData (ConstInt _ dat)     = dat
--tokenData (Keyword _ dat)      = dat
--tokenData (OpTok _ dat)        = dat


buildTokMsg :: Token -> String
buildTokMsg t = "'" ++ toStringToken t ++ "'"


toStringToken :: Token -> String
toStringToken tok =
        case tok of
             SemiColon _                   -> ";"
             Colon _                       -> ":"
             QuestMark _                   -> "?"
             Comma _                       -> ","
             OpenBracket OpenParen _       -> "("
             OpenBracket OpenBrace _       -> "{"
             OpenBracket OpenSqBracket _   -> "["
             CloseBracket CloseBrace _     -> "}"
             CloseBracket CloseParen _     -> ")"
             CloseBracket CloseSqBracket _ -> "]"
             Ident a _                     -> a
             ConstInt n _                  -> show n
             Keyword kwd _                 -> map toLower (show kwd)
             OpTok op _                    -> toStringOpTok op


toStringOpTok :: OpTok -> String
toStringOpTok opTok =
        case opTok of
             PlusSign          -> "+"
             MinusSign         -> "-"
             Asterisk          -> "*"
             Backslash         -> "/"
             Percent           -> "%"
             Tilde             -> "~"
             Bang              -> "!"
             PipePipe          -> "||"
             AmpAmp            -> "&&"
             RightArrow        -> ">"
             RightArrowEqual   -> ">="
             LeftArrow         -> "<"
             LeftArrowEqual    -> "<="
             EqualEqual        -> "=="
             BangEqual         -> "!="
             EqualSign         -> "="
             PlusEqual         -> "+="
             MinusEqual        -> "-="
             AsteriskEqual     -> "*="
             BackslashEqual    -> "/="
             PercentEqual      -> "%="
             Ampersand         -> "&"
             PlusPlus          -> "++"
             MinusMinus        -> "--"
             Caret             -> "^"
             Pipe              -> "|"
             AmpEqual          -> "&="
             CaretEqual        -> "^="
             PipeEqual         -> "|="
             DoubleLeftArrow   -> "<<"
             DoubleRightArrow  -> ">>"
             DoubleLArrowEqual -> "<<="
             DoubleRArrowEqual -> ">>="
