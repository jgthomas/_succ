
module Builder.BuildState
        (BuildState,
         evaluate,
         throwError,
         getState,
         putState
        ) where


import Options         (SuccOptions (..))
import Types.SuccState (SuccStateM, evaluate, getState, putState, throwError)


-- | State definition
type BuildState = SuccStateM SuccOptions
