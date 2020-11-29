-- |
-- Module       : MessageTypeError
-- Description  : Format type error messages
--
-- Formats error messages of the type error type.
module PrintError.MessageTypeError
  ( typeErrorMsg,
  )
where

import PrintError.PrintErrorTokens (PrintRange (..), buildLineMsg)
import Types.AST (NodeDat (..), Tree (..))
import Types.Error (TypeError (..))
import Types.Type (Type)

-- | Generate type error message
typeErrorMsg :: TypeError -> (String, PrintRange)
typeErrorMsg (TypeMismatch a b (FunctionNode _ name _ _ dat)) =
  (msg, Exact (startLine dat))
  where
    msg =
      buildLineMsg (startLine dat)
        <> "Parameter type mismatch between declarations of '"
        <> name
        <> "' was '"
        <> typeString a
        <> "' now '"
        <> typeString b
        <> "'"
typeErrorMsg (TypeMismatch a b (AssignmentNode (VarNode name _) _ _ dat)) =
  (msg, Exact $ startLine dat)
  where
    msg =
      buildLineMsg (startLine dat)
        <> "Type mismatch for '"
        <> name
        <> "' between declaration '"
        <> typeString a
        <> "' and assignment '"
        <> typeString b
typeErrorMsg err = (show err, All)

typeString :: [Type] -> String
typeString ts = unwords . map show $ ts
