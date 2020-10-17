-- |
-- Module       : MessageLogicError
-- Description  : Format logic error messages
--
-- Formats error messages of the logic error type.
module PrintError.MessageLogicError
  ( logicErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..))
import Types.Error (LogicError (..))

-- | Generate logic error message
logicErrorMsg :: LogicError -> (String, PrintRange)
logicErrorMsg err = (show err, All)
