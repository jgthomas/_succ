module Builder.BuildState
  ( BuildState,
    evaluate,
    throwError,
    getState,
    putState,
  )
where

import Types.SuccState (SuccStateM, evaluate, getState, putState, throwError)
import Types.SuccTokens (SuccOptions (..))

-- | State definition
type BuildState = SuccStateM SuccOptions
