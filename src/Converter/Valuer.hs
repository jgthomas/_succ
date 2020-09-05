
module Converter.Valuer
        (storeValue,
         adjustVariable,
         checkValueIncDec,
         argsToPosValue
        ) where


import           Control.Monad   (unless)

import qualified Analyser.Valuer as Valuer (value)
import           State.GenState  (GenState)
import qualified State.State     as State (memOffset, setVariableValue)
import           Types.AST       (NodeDat (..), Tree (..))
import           Types.Operator
import           Types.Variables (VarType (..), VarValue (..))


-- | Determine the value of a list of nodes
argsToPosValue :: [Tree] -> GenState [(Int, VarValue)]
argsToPosValue argList = zip [0..] <$> mapM Valuer.value argList


-- | Store the value of a variable
storeValue :: NodeDat -> String -> Tree -> GenState ()
storeValue dat varName valNode = setValue dat varName valNode 0


setValue :: NodeDat -> String -> Tree -> Int -> GenState ()
setValue dat varName valNode n =
        unless (isSkipped dat) $
             if notTracked dat
                then State.setVariableValue varName UntrackedValue
                else do
                        varValue <- Valuer.value valNode
                        State.setVariableValue varName (changeValue varValue n)


changeValue :: VarValue -> Int -> VarValue
changeValue (SingleValue n) m = SingleValue (n + m)
changeValue varValue _        = varValue


-- | Adjust the value of a variable
adjustVariable :: Maybe Int -> Maybe Int -> VarType -> VarType
adjustVariable (Just x) (Just y) (LocalVar n _ _) = LocalVar n (x * State.memOffset) y
adjustVariable (Just x) Nothing (LocalVar n _ sp) =
        LocalVar n (x * State.memOffset) (sp + (x * (-State.memOffset)))
adjustVariable Nothing (Just y) (LocalVar n m _)  = LocalVar n m y
adjustVariable (Just x) _ (ParamVar n _)          = ParamVar n x
adjustVariable (Just x) _ (GlobalVar l _)         = GlobalVar l x
adjustVariable _ _ varType                        = varType


-- | Adjust the value of a pre- or post-incremented variable
checkValueIncDec :: Tree -> GenState ()
checkValueIncDec (UnaryNode valNode@(VarNode name _) (PreOpUnary PreIncrement) dat) =
        setValue dat name valNode 1
checkValueIncDec (UnaryNode valNode@(VarNode name _) (PreOpUnary PreDecrement) dat) =
        setValue dat name valNode (-1)
checkValueIncDec (UnaryNode valNode@(VarNode name _) (PostOpUnary PostIncrement) dat) =
        setValue dat name valNode 1
checkValueIncDec (UnaryNode valNode@(VarNode name _) (PostOpUnary PostDecrement) dat) =
        setValue dat name valNode (-1)
checkValueIncDec _ = pure ()
