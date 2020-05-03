
module Error.MessageTypeError (typeErrorMsg) where


import Error.Error            (TypeError (..))
import Error.PrintErrorTokens (PrintRange (..), buildLineMsg)
import Types.AST              (NodeDat (..), Tree (..))
import Types.Type             (Type)


typeErrorMsg :: TypeError -> (String, PrintRange)

typeErrorMsg (TypeMismatch a b (FunctionNode _ name _ _ dat)) =
        (msg, Exact (startLine dat))
        where msg = buildLineMsg (startLine dat)
                    ++ "Parameter type mismatch between declarations of '" ++ name
                    ++ "' was '" ++ typeString a
                    ++ "' now '" ++ typeString b ++ "'"

typeErrorMsg (TypeMismatch a b (AssignmentNode (VarNode name _) _ _ dat)) =
        (msg, Exact $ startLine dat)
        where msg = buildLineMsg (startLine dat)
                    ++ "Type mismatch for '" ++ name
                    ++ "' between declaration '" ++ typeString a
                    ++ "' and assignment '" ++ typeString b

typeErrorMsg err = (show err, All)


typeString :: [Type] -> String
typeString ts = unwords . map show $ ts
