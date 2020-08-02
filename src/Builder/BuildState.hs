
module Builder.BuildState
        (BuildState,
         runBuildState,
         throwError,
         getState,
         putState,
         startState
        ) where


import           Types.Error     (CompilerError)
import           Types.Optimise  (Optimise (..))
import           Types.SuccState (SuccStateM, getState, putState, throwError)
import qualified Types.SuccState as SuccState (runSuccState)


-- | State definition
type BuildState = SuccStateM Optimise


-- | Initial state
startState ::  Optimise
startState = OptimiseOn


-- | Run the state extracting the error or result
runBuildState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runBuildState f t s = SuccState.runSuccState f t s
