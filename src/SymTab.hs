
module SymTab (currentScope,
               labelNum,
               module GlobalScope,
               module FuncState) where


import Evaluator   (Evaluator(Ev))
import Types       (SymTab(label))
import FrameStack  (currentScope)
import GlobalScope hiding (globalType, declaredFuncType)
import FuncState   hiding (allTypes, variableType, parameterType)


labelNum :: Evaluator Int
labelNum = Ev $ \symTab ->
        (label symTab, symTab { label = succ . label $ symTab })
