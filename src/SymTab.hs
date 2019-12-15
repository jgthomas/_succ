
module SymTab
        (getScope,
         labelNum,
         module Global,
         module Local
        ) where


import           FrameStack (getScope)
import           GenState   (GenState)
import qualified GenState   (getLabel, putLabel)
import           Global     hiding (declaredFuncType, globalType)
import           Local      hiding (allTypes, parameterType, variableType)


labelNum :: GenState Int
labelNum = do
        label <- GenState.getLabel
        GenState.putLabel . succ $ label
        return label
