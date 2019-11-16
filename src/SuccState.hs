
module SuccState where


import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT)

import Error (CompilerError)


type CompilerM m = ExceptT CompilerError m
