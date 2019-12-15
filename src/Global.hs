{-|
Module       : Global
Description  : Track global scope state

Keeps track of all global variables and functions defined in the program.
-}
module Global
        (declareFunction,
         defineFunction,
         declareGlobal,
         decParamCount,
         decSeqNumber,
         currentSeqNumber,
         globalLabel,
         globalType,
         declaredFuncType,
         checkVarDefined,
         checkFuncDefined,
         getUndefined,
         storeForInit,
         getAllForInit,
         defineGlobal
        ) where


import qualified Data.Map    as M
import qualified Data.Set    as S

import qualified FrameStack  (currentFunc)
import           GenState    (GenState)
import qualified GenState    (getGlobalScope, putGlobalScope)
import           GlobalScope (GlobalScope (..), GlobalVar (..))
import qualified GlobalScope (mkGloVar)
import           Type        (Type)


-- | Get the number of parameters for a declared function
decParamCount :: String -> GenState (Maybe Int)
decParamCount name = lookUp name funcParams


-- | Get the declaration sequence number
decSeqNumber :: String -> GenState (Maybe Int)
decSeqNumber name = lookUp name funcDecSeq


-- | Get the ASM label associated with a named variable
globalLabel :: String -> GenState (Maybe String)
globalLabel name = extract globLabel <$> lookUp name declaredVars


-- | Get the type of a named variable
globalType :: String -> GenState (Maybe Type)
globalType name = extract globType <$> lookUp name declaredVars


-- | Get the type of a declared function
declaredFuncType :: String -> GenState (Maybe Type)
declaredFuncType name = lookUp name funcTypes


-- | Get declaration number of current function
currentSeqNumber :: GenState (Maybe Int)
currentSeqNumber = do
        currFunc <- FrameStack.currentFunc
        decSeqNumber currFunc


-- | Check if a variable has been defined
checkVarDefined :: String -> GenState Bool
checkVarDefined name = S.member name . definedVars <$> GenState.getGlobalScope


-- | Check if a function has been defined
checkFuncDefined :: String -> GenState Bool
checkFuncDefined name = S.member name . definedFuncs <$> GenState.getGlobalScope


-- | Declare a function
declareFunction :: Type -> String -> Int -> GenState ()
declareFunction typ funcName paramCount = do
        gscope <- GenState.getGlobalScope
        let gscope'   = addSymbol funcName gscope
            gscope''  = addParams funcName paramCount gscope'
            gscope''' = addType funcName typ gscope''
        GenState.putGlobalScope gscope'''


-- | Define a function
defineFunction :: String -> GenState ()
defineFunction name = do
        gscope <- GenState.getGlobalScope
        GenState.putGlobalScope $ funcAsDefined name gscope


-- | Declare a global variable
declareGlobal :: String -> Type -> String -> GenState ()
declareGlobal name typ label = do
        gscope <- GenState.getGlobalScope
        let globVar = GlobalScope.mkGloVar label typ
        GenState.putGlobalScope $ addGlobal name globVar gscope


-- | Define a global variable
defineGlobal :: String -> GenState ()
defineGlobal name = do
        gscope <- GenState.getGlobalScope
        GenState.putGlobalScope $ varAsDefined name gscope


-- | Get list of all undefined global variables
getUndefined :: GenState [String]
getUndefined = do
        gscope <- GenState.getGlobalScope
        let definedSet   = definedVars gscope
            declaredSet  = M.keysSet $ declaredVars gscope
            undefinedSet = S.difference declaredSet definedSet
        map globLabel
            . M.elems
            . M.filterWithKey (\k _ -> k `elem` undefinedSet)
            . declaredVars
            <$> GenState.getGlobalScope


-- | Store ASM code to initialise a global variable
storeForInit :: String -> GenState ()
storeForInit code = do
        gscope <- GenState.getGlobalScope
        let gscope' = gscope { varsToInit = code : varsToInit gscope }
        GenState.putGlobalScope gscope'


-- | Get list of all stored initilisation ASM code
getAllForInit :: GenState [String]
getAllForInit = varsToInit <$> GenState.getGlobalScope


lookUp :: (Ord k) => k -> (GlobalScope -> M.Map k a) -> GenState (Maybe a)
lookUp n f = M.lookup n . f <$> GenState.getGlobalScope


extract :: (GlobalVar -> a) -> Maybe GlobalVar -> Maybe a
extract f (Just gv) = Just . f $ gv
extract _ Nothing   = Nothing


addGlobal :: String -> GlobalVar -> GlobalScope -> GlobalScope
addGlobal n g s = s { declaredVars = M.insert n g $ declaredVars s }


addParams :: String -> Int -> GlobalScope -> GlobalScope
addParams n p s = s { funcParams = M.insert n p $ funcParams s }


addType :: String -> Type -> GlobalScope -> GlobalScope
addType n t s = s { funcTypes = M.insert n t $ funcTypes s }


varAsDefined :: String -> GlobalScope -> GlobalScope
varAsDefined n s = s { definedVars = S.insert n $ definedVars s }


funcAsDefined :: String -> GlobalScope -> GlobalScope
funcAsDefined n s = s { definedFuncs = S.insert n $ definedFuncs s }


addSymbol :: String -> GlobalScope -> GlobalScope
addSymbol name gs =
        let i   = seqNum gs
            gs' = gs { seqNum = i + 1 }
            in
        gs' { funcDecSeq = M.insert name i $ funcDecSeq gs' }
