
module MessageOtherError (stateErrorMsg, impossibleErrorMsg) where


import Error            (StateError (..))
import PrintErrorTokens (PrintRange (..))


stateErrorMsg :: StateError -> (String, PrintRange)

stateErrorMsg (NoStateFound name) = (msg, None)
        where msg = "Unable to locate any state for '" ++ name
                    ++ "' compilation terminated"

stateErrorMsg (UndefinedScope name scope) = (msg, None)
        where msg = "Unable to locate state for scope '" ++ show scope
                    ++ "' in '" ++ name ++ "' compilation terminated"


impossibleErrorMsg :: (String, PrintRange)
impossibleErrorMsg = (msg, None)
        where msg = "Something unexpected went wrong, you are on your own!"
