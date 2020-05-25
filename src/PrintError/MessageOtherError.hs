{-|
Module       : MessageOtherError
Description  : Format general error messages

Formats general error messages.
-}
module PrintError.MessageOtherError (impossibleErrorMsg) where


import PrintError.PrintErrorTokens (PrintRange (..))


-- | Generate impossible error message
impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = (msg, None)
        where msg = "Something unexpected went wrong, you are on your own!"
