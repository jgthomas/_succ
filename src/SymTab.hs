
module SymTab
        (module FrameStack,
         module Global,
         module Local,
         module GenState
        ) where


import FrameStack (getScope)
import GenState   (labelNum)
import Global     hiding (declaredFuncType, globalType)
import Local      hiding (allTypes, parameterType, variableType)
