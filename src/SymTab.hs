
module SymTab
        (getScope,
         labelNum,
         module GlobalScope,
         module FuncState
        ) where


import           FrameStack  (getScope)
import           FuncState   hiding (allTypes, parameterType, variableType)
import           GenState    (GenState)
import qualified GenState    (getLabel, putLabel)
import           GlobalScope hiding (declaredFuncType, globalType)


labelNum :: GenState Int
labelNum = do
        label <- GenState.getLabel
        GenState.putLabel . succ $ label
        return label
