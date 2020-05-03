
module Error.PrintErrorTokens where


import Tokens (Token (..))


data PrintRange = All
                | None
                | Exact Int
                | Range Int Int
                | From Int
                | Until Int
                deriving (Eq)


buildLineMsg :: Int -> String
buildLineMsg n = "Line " ++ show n ++ ": "


buildTokMsg :: Token -> String
buildTokMsg t = "'" ++ show t ++ "'"


