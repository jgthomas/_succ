
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
