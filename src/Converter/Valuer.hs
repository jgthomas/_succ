{-|
Module       : Valuer
Description  : Evaluates tree values

Determines the value associated with a node in the syntax tree
-}
module Converter.Valuer (variableValue) where


import Types.AST       (Tree (..))
import Types.Variables (VarValue (..))


-- | Determine a tree node value
variableValue :: Tree -> VarValue
variableValue (ConstantNode n _) = SingleValue n
variableValue _                  = UntrackedValue
