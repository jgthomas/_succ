
module SymbolTable (Evaluator(..),
                    SymTab(..),
                    FuncState(..),
                    FuncStates(..),
                    LocalScope,
                    FunctionScope,
                    ProgramScope
                   )
        where


import SimpleStack
import qualified Declarations as Dec
import qualified Data.Map as M
import Control.Monad (liftM, ap)


newtype Evaluator a = Ev (SymTab -> (a, SymTab))

{-
-  Monad is now defined as a subclass of applicative
-  and so new monad definitions require that they also
-  be specified as applicatives (and functors, of which
-  applicatives are a subclass)
-
-  See: https://stackoverflow.com/a/34641501/5671759
-}
instance Functor Evaluator where
        fmap = liftM


instance Applicative Evaluator where
        pure  = return
        (<*>) = ap


instance Monad Evaluator where
        -- return :: a -> ma
        return a = Ev (\symTab -> (a, symTab))

        -- (>>=)  :: ma -> (a -> mb) -> mb
        (Ev act) >>= k = Ev $ \symTab ->
                let (a, symTab') = act symTab
                    (Ev act')    = k a
                    in
                act' symTab'


type LocalScope = M.Map String Int
type FunctionScope = M.Map Int LocalScope
type ProgramScope = M.Map String FunctionScope

{-
- State of the whole program
-
- labelNo      : the number of the next label to output in asm
- offset       : the offset from %rbp where the next local variable should be stored
- nameStack    : stack of program function calls, current function at top
- declarations : key=function name, value=number indicating sequence of when declared
- funcStates   : key=function name, value=state container for that function
- scopeLevels  : key=function name, value=current scope depth in that function
- scopesData   : triply nested map tracking the variables in each scope
-    ProgramScope
-          key=function name, value=map of the scopes in each function
-    FunctionScope
-          key=scope level in function, value=map of variables in that scope
-    LocalScope
-          key=variable name, value=offset from %rbp where stored
-
-}
data SymTab = Tab { labelNo      :: Int
                  , offset       :: Int
                  , nameStack    :: Stack String
                  , declarations :: Dec.Declared
                  , funcStates   :: FuncStates
                  , scopeLevels  :: M.Map String Int
                  , scopesData   :: ProgramScope }
            deriving Show


type FuncParams = M.Map String Int
type FuncStates = M.Map String FuncState

{-
- State of a function
-
- paramCount : the number of parameters the function has
- argCount   : counter for arguments passed
- parameters : key=parameter name, value=parameter position
-
-}
data FuncState = Fs { paramCount :: Int
                    , argCount   :: Int
                    , parameters :: FuncParams }
               deriving Show
