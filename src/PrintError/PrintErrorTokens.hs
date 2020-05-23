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


-- | Builds a message about a token involved in an error
buildTokMsg :: Token -> String
buildTokMsg t = "'" ++ toStringToken t ++ "'"


toStringToken :: Token -> String
toStringToken tok =
        case tok of
             SemiColon                     -> ";"
             Colon                         -> ":"
             QuestMark                     -> "?"
             Comma                         -> ","
             (OpenBracket OpenParen)       -> "("
             (OpenBracket OpenBrace)       -> "{"
             (OpenBracket OpenSqBracket)   -> "["
             (CloseBracket CloseBrace)     -> "}"
             (CloseBracket CloseParen)     -> ")"
             (CloseBracket CloseSqBracket) -> "]"
             (Ident a)                     -> a
             (ConstInt n)                  -> show n
             (Keyword kwd)                 -> map toLower (show kwd)
             (OpTok op)                    -> toStringOpTok op


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
