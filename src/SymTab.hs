
module SymTab (currentScope,
               labelNum,
               module GlobalScope,
               module FuncState) where


import Types       (SymTab(label))
import FrameStack  (currentScope)
import GlobalScope hiding (globalType, declaredFuncType)
import FuncState   hiding (allTypes, variableType, parameterType)
import SuccState   (GenState, getState, putState)


labelNum :: GenState Int
labelNum = do
        state <- getState
        putState $ state { label = succ . label $ state }
        return $ label state

