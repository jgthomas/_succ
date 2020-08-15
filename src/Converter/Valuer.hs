
module Converter.Valuer (variableValue) where


import Types.AST       (Tree (..))
import Types.Variables (VarValue (..))


variableValue :: Tree -> VarValue
variableValue (ConstantNode n _) = SingleValue n
variableValue _                  = UntrackedValue
