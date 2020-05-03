{-|
Module       : SymbolTable
Description  : State data type for code generation

Data type holding the state as the checker and code generator
traverse the abstract syntax tree.
-}
module SymbolTable where


import qualified Data.Map   as M
import qualified Data.Set   as S

import           Types.Type (Type)


-- | SymTab state data type
data SymTab = SymTab { label       :: Int
                     , frameStack  :: Stack String
                     , globalScope :: GlobalScope
                     , funcStates  :: M.Map String FuncState }
            deriving (Show)


-- | SymTab state constructor
mkSymTab :: SymTab
mkSymTab = SymTab 1 mkStack mkGlobalScope M.empty


-- | Global scope state data type
data GlobalScope = Gscope { seqNum       :: Int
                          , funcDecSeq   :: M.Map String Int
                          , funcParams   :: M.Map String Int
                          , funcTypes    :: M.Map String Type
                          , declaredVars :: M.Map String GlobalVar
                          , definedVars  :: S.Set String
                          , definedFuncs :: S.Set String
                          , varsToInit   :: [String] }
                 deriving (Show)


-- | Global variable state data type
data GlobalVar = GloVar { globLabel :: String
                        , globType  :: Type }
               deriving (Show)


-- | Global scope state constructor
mkGlobalScope :: GlobalScope
mkGlobalScope = Gscope 0 M.empty M.empty M.empty M.empty S.empty S.empty []


-- | Global variable state constructor
mkGloVar :: String -> Type -> GlobalVar
mkGloVar l t = GloVar l t


-- | Local scope data type
data FuncState = Fs { paramCount   :: Int
                    , funcOffset   :: Int
                    , currentScope :: Int
                    , parameters   :: M.Map String ParamVar
                    , scopes       :: M.Map Int (M.Map String LocalVar) }
               deriving (Show)


-- | Local variable state data type
data LocalVar = LocVar { locOffset :: Int
                       , locType   :: Type }
              deriving (Show)


-- | Parameter state data type
data ParamVar = ParVar { paramNum  :: Int
                       , paramType :: Type }
              deriving (Show)


-- | Local scope state constructor
mkFuncState :: FuncState
mkFuncState = Fs 0 memOffset 0 M.empty (M.singleton 0 M.empty)


-- | Local variable state constructor
mkLocVar :: Int -> Type -> LocalVar
mkLocVar n t = LocVar n t


-- | Parameter state constructor
mkParVar :: Int -> Type -> ParamVar
mkParVar n t = ParVar n t


-- | Offset step from the stack frame base pointer
memOffset :: Int
memOffset = -8


-- | Stack definition
newtype Stack a = Stack [a] deriving Show


-- | Stack constructor
mkStack :: Stack a
mkStack = Stack []


-- | Push element onto stack
stackPush :: a -> Stack a -> Stack a
stackPush x (Stack s) = Stack (x:s)


-- | Pop element off stack
stackPop :: Stack a -> Stack a
stackPop (Stack []) = Stack []
stackPop (Stack s)  = Stack $ tail s


-- | Look at top element
stackPeek :: Stack a -> Maybe a
stackPeek (Stack []) = Nothing
stackPeek (Stack s)  = Just $ head s
