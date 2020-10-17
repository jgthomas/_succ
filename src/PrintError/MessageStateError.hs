-- |
-- Module       : MessageStateError
-- Description  : Format state error messages
--
-- Formats error messages of the state error type.
module PrintError.MessageStateError
  ( stateErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..))
import Types.Error (StateError (..))

-- | Generate state error message
stateErrorMsg :: StateError -> (String, PrintRange)
stateErrorMsg (NoStateFound name) = (msg, None)
  where
    msg =
      "Unable to locate any state for '" ++ name
        ++ "' compilation terminated"
stateErrorMsg (UndefinedScope name scope) = (msg, None)
  where
    msg =
      "Unable to locate state for scope '" ++ show scope
        ++ "' in '"
        ++ name
        ++ "' compilation terminated"
