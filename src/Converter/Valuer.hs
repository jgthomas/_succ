
module Converter.Valuer
        (storeValue,
         checkValueIncDec,
         argsToPosValue
        ) where


import           Control.Monad   (unless)

import qualified Analyser.Valuer as Valuer (value)
import           State.GenState  (GenState)
import qualified State.State     as State (setVariableValue)
import           Types.AST       (NodeDat (..), Tree (..))
import           Types.Operator
import           Types.Variables (VarValue (..))


-- | Determine the value of a list of nodes
argsToPosValue :: [Tree] -> GenState [(Int, VarValue)]
argsToPosValue argList = zip [0..] <$> mapM Valuer.value argList


-- | Store the value of a node
storeValue :: NodeDat -> String -> Tree -> GenState ()
storeValue dat varName valNode = setValue dat varName valNode 0


-- | Adjust the stored value of a pre- or post-incremented node
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
