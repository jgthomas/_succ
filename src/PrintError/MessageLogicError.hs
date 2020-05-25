
module PrintError.MessageLogicError (logicErrorMsg) where


import PrintError.PrintErrorTokens (PrintRange (..))
import Types.Error                 (LogicError (..))


logicErrorMsg :: LogicError -> (String, PrintRange)
logicErrorMsg err = (show err, All)
