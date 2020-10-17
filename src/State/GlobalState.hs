-- |
-- Module       : GlobalState
-- Description  : Control state for the global scope
--
-- Functions to manipulate the state stored for the global scope of a program.
module State.GlobalState
  ( declareFunction,
    defineFunction,
    declareGlobal,
    decParamCount,
    decSeqNumber,
    currentSeqNumber,
    getLabel,
    getType,
    getValue,
    setValue,
    declaredFuncType,
    checkVarDefined,
    checkFuncDefined,
    getUndefinedVarData,
    defineGlobal,
    makeLabel,
    incrementDecSeq,
    previouslyDeclaredFunc,
    previouslyDeclaredVar,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified State.FrameStack as FrameStack (currentFunc)
import State.GenState (GenState, throwError)
import qualified State.GenState as GenState
  ( getGlobalScope,
    labelNum,
    putGlobalScope,
  )
import State.SymbolTable (GlobalScope (..), GlobalVar (..))
import qualified State.SymbolTable as SymbolTable (mkGloVar)
import Types.Error (CompilerError (StateError), StateError (..))
import Types.Type (Type)
import Types.Variables (VarValue (..))

-- | Get the number of parameters for a declared function
decParamCount :: String -> GenState (Maybe Int)
decParamCount name = lookUp name funcParams

-- | Get the declaration sequence number
decSeqNumber :: String -> GenState (Maybe Int)
decSeqNumber name = lookUp name funcDecSeq

-- | Check whether a function has been declared before
previouslyDeclaredFunc :: String -> GenState Bool
previouslyDeclaredFunc name = do
  n <- decSeqNumber name
  case n of
    Nothing -> pure False
    Just _ -> pure True

-- | Check whether a variable has been declared before
previouslyDeclaredVar :: String -> GenState Bool
previouslyDeclaredVar name = do
  label <- getLabel name
  case label of
    Nothing -> pure False
    Just _ -> pure True

-- | Get the assembly label associated with a named variable
getLabel :: String -> GenState (Maybe String)
getLabel name = extract globLabel <$> getGlobalVar name

-- | Get the type of a named variable
getType :: String -> GenState (Maybe Type)
getType name = extract globType <$> getGlobalVar name

-- | Get the current value of a named variable
getValue :: String -> GenState (Maybe VarValue)
getValue name = extract globValue <$> getGlobalVar name

-- | Update value of named variable
setValue :: String -> VarValue -> GenState ()
setValue name value = do
  globVar <- getGlobalVar name
  case globVar of
    Nothing -> throwError $ StateError (NoStateFound name)
    Just gv -> setGlobalVar name $ gv {globValue = value}

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
  gscope' <- addSymbol funcName gscope
  gscope'' <- addParams funcName paramCount gscope'
  gscope''' <- addType funcName typ gscope''
  putGlobalScope gscope'''

-- | Define a function
defineFunction :: String -> GenState ()
defineFunction name = do
  gscope <- getGlobalScope
  let definedFuncs' = S.insert name . definedFuncs $ gscope
  putGlobalScope $ gscope {definedFuncs = definedFuncs'}

-- | Declare a global variable
declareGlobal :: String -> Type -> String -> GenState ()
declareGlobal name typ label = do
  gscope <- getGlobalScope
  let globVar = SymbolTable.mkGloVar label typ
      declaredVars' = M.insert name globVar $ declaredVars gscope
  putGlobalScope $ gscope {declaredVars = declaredVars'}

-- | Define a global variable
defineGlobal :: String -> GenState ()
defineGlobal name = do
  gscope <- getGlobalScope
  let definedVars' = S.insert name . definedVars $ gscope
  putGlobalScope $ gscope {definedVars = definedVars'}

-- | Create label for global variable
makeLabel :: String -> GenState String
makeLabel name = do
  labnum <- GenState.labelNum
  pure $ "_" ++ name ++ show labnum

-- | Get list of all undefined global variable data
getUndefinedVarData :: GenState [(String, Type)]
getUndefinedVarData = do
  undefinedSet <- getUndefinedVarNames
  map getGlobalData
    . M.elems
    . M.filterWithKey (\k _ -> k `elem` undefinedSet)
    . declaredVars
    <$> getGlobalScope

-- | Change declaration sequence number on redeclaration
incrementDecSeq :: GenState ()
incrementDecSeq = do
  currFunc <- FrameStack.currentFunc
  gscope <- getGlobalScope
  gscope' <- addSymbol currFunc gscope
  putGlobalScope gscope'

getGlobalData :: GlobalVar -> (String, Type)
getGlobalData (GloVar label typ _) = (label, typ)

getUndefinedVarNames :: GenState (S.Set String)
getUndefinedVarNames = do
  gscope <- getGlobalScope
  let definedSet = definedVars gscope
      declaredSet = M.keysSet $ declaredVars gscope
  pure $ S.difference declaredSet definedSet

getGlobalVar :: String -> GenState (Maybe GlobalVar)
getGlobalVar name = extract id <$> lookUp name declaredVars

setGlobalVar :: String -> GlobalVar -> GenState ()
setGlobalVar name globalVar = do
  gscope <- getGlobalScope
  let declaredVars' = M.insert name globalVar $ declaredVars gscope
  putGlobalScope $ gscope {declaredVars = declaredVars'}

lookUp :: (Ord k) => k -> (GlobalScope -> M.Map k a) -> GenState (Maybe a)
lookUp n f = M.lookup n . f <$> getGlobalScope

extract :: (b -> a) -> Maybe b -> Maybe a
extract f (Just pv) = Just . f $ pv
extract _ Nothing = Nothing

checkInSet :: (Ord a) => a -> (GlobalScope -> S.Set a) -> GenState Bool
checkInSet a f = S.member a . f <$> getGlobalScope

addParams :: String -> Int -> GlobalScope -> GenState GlobalScope
addParams n p s = pure $ s {funcParams = M.insert n p $ funcParams s}

addType :: String -> Type -> GlobalScope -> GenState GlobalScope
addType n t s = pure $ s {funcTypes = M.insert n t $ funcTypes s}

addSymbol :: String -> GlobalScope -> GenState GlobalScope
addSymbol n g =
  let g' = g {funcDecSeq = M.insert n (seqNum g) $ funcDecSeq g}
   in pure $ g' {seqNum = succ . seqNum $ g'}

getGlobalScope :: GenState GlobalScope
getGlobalScope = GenState.getGlobalScope

putGlobalScope :: GlobalScope -> GenState ()
putGlobalScope gscope = GenState.putGlobalScope gscope
