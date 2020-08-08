
module Builder.BuildState
        (BuildState,
         runBuildState,
         throwError,
         getState,
         putState
        ) where


import           Types.Error      (CompilerError)
import           Types.SuccState  (SuccStateM, getState, putState, throwError)
import qualified Types.SuccState  as SuccState (runSuccState)
import           Types.SuccTokens (Optimise (..))


-- | State definition
type BuildState = SuccStateM Optimise


-- | Run the state extracting the error or result
runBuildState :: (t -> SuccStateM s a) -> t -> s -> Either CompilerError a
runBuildState f t s = SuccState.runSuccState f t s
