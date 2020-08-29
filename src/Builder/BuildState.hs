
module Builder.BuildState
        (BuildState,
         evaluate,
         throwError,
         getState,
         putState
        ) where


import Types.SuccState  (SuccStateM, evaluate, getState, putState, throwError)
import Types.SuccTokens (Optimise (..))


-- | State definition
type BuildState = SuccStateM Optimise
