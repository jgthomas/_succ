
module GenTokens where


data Scope = Global
           | Local
           deriving (Eq)


data VarLookup = NotFound
               | VarType VarType
               deriving (Eq)


data VarType = LocalVar Int Int
             | ParamVar Int Int
             | GlobalVar String Int
             deriving (Eq)


mkVarLocal :: Maybe Int -> VarLookup
mkVarLocal (Just n) = VarType (LocalVar n 0)
mkVarLocal Nothing  = NotFound


mkVarParam :: Maybe Int -> VarLookup
mkVarParam (Just n) = VarType (ParamVar n 0)
mkVarParam Nothing  = NotFound


mkVarGlobal :: Maybe String -> VarLookup
mkVarGlobal (Just s) = VarType (GlobalVar s 0)
mkVarGlobal Nothing  = NotFound
