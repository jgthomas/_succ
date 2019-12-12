
module SymTab
        (currentScope,
         labelNum,
         module GlobalScope,
         module FuncState
        ) where


import FrameStack         (currentScope)
import GenState           (GenState)
import qualified GenState (getLabel, putLabel)
import GlobalScope hiding (globalType, declaredFuncType)
import FuncState   hiding (allTypes, variableType, parameterType)


labelNum :: GenState Int
labelNum = do
        label <- GenState.getLabel
        GenState.putLabel . succ $ label
        return label
