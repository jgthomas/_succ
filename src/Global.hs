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
         defineGlobal,
         mkGlobLabel
        ) where


import qualified Data.Map    as M
import qualified Data.Set    as S

import qualified FrameStack  (currentFunc)
import           GenState    (GenState)
import qualified GenState    (getGlobalScope, labelNum, putGlobalScope)
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
checkVarDefined name = checkInSet name definedVars


-- | Check if a function has been defined
checkFuncDefined :: String -> GenState Bool
checkFuncDefined name = checkInSet name definedFuncs


-- | Declare a function
declareFunction :: Type -> String -> Int -> GenState ()
declareFunction typ funcName paramCount = do
        gscope <- getGlobalScope
        let gscope'   = addSymbol funcName gscope
            gscope''  = addParams funcName paramCount gscope'
            gscope''' = addType funcName typ gscope''
        GenState.putGlobalScope gscope'''


-- | Define a function
defineFunction :: String -> GenState ()
defineFunction name = do
        gscope <- getGlobalScope
        let definedFuncs' = S.insert name . definedFuncs $ gscope
        GenState.putGlobalScope $ gscope { definedFuncs = definedFuncs' }


-- | Declare a global variable
declareGlobal :: String -> Type -> String -> GenState ()
declareGlobal name typ label = do
        gscope <- getGlobalScope
        let globVar       = GlobalScope.mkGloVar label typ
            declaredVars' = M.insert name globVar $ declaredVars gscope
        GenState.putGlobalScope $ gscope { declaredVars = declaredVars' }


-- | Define a global variable
defineGlobal :: String -> GenState ()
defineGlobal name = do
        gscope <- getGlobalScope
        let definedVars' = S.insert name . definedVars $ gscope
        GenState.putGlobalScope $ gscope { definedVars = definedVars' }


-- | Get list of all undefined global variables
getUndefined :: GenState [String]
getUndefined = do
        gscope <- getGlobalScope
        let definedSet   = definedVars gscope
            declaredSet  = M.keysSet $ declaredVars gscope
            undefinedSet = S.difference declaredSet definedSet
        pure $ map globLabel
            . M.elems
            . M.filterWithKey (\k _ -> k `elem` undefinedSet)
            . declaredVars
            $ gscope


-- | Store ASM code to initialise a global variable
storeForInit :: String -> GenState ()
storeForInit code = do
        gscope <- getGlobalScope
        let gscope' = gscope { varsToInit = code : varsToInit gscope }
        GenState.putGlobalScope gscope'


-- | Get list of all stored initilisation ASM code
getAllForInit :: GenState [String]
getAllForInit = varsToInit <$> getGlobalScope


-- | TODO
mkGlobLabel :: String -> GenState String
mkGlobLabel name = do
        labnum <- GenState.labelNum
        pure $ "_" ++ name ++ show labnum


lookUp :: (Ord k) => k -> (GlobalScope -> M.Map k a) -> GenState (Maybe a)
lookUp n f = M.lookup n . f <$> getGlobalScope


extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing   = Nothing


checkInSet :: (Ord a) => a -> (GlobalScope -> S.Set a) -> GenState Bool
checkInSet a f = S.member a . f <$> getGlobalScope


addParams :: String -> Int -> GlobalScope -> GlobalScope
addParams n p s = s { funcParams = M.insert n p $ funcParams s }


addType :: String -> Type -> GlobalScope -> GlobalScope
addType n t s = s { funcTypes = M.insert n t $ funcTypes s }


addSymbol :: String -> GlobalScope -> GlobalScope
addSymbol n g =
        let g' = g { funcDecSeq = M.insert n (seqNum g) $ funcDecSeq g }
            in
        g' { seqNum = succ . seqNum $ g' }


getGlobalScope :: GenState GlobalScope
getGlobalScope = GenState.getGlobalScope
