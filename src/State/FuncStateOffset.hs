
module State.FuncStateOffset
        (currentOffset,
         incrementOffsetByN,
         stackPointerValue
        ) where


import qualified State.FrameStack      as FrameStack (currentFunc)
import           State.FuncStateAccess (getFuncState, setFuncState)
import           State.GenState        (GenState)
import           State.SymbolTable     (FuncState (funcOffset), memOffset)


-- | Retrieve current value of stack pointer
stackPointerValue :: GenState Int
stackPointerValue = negate <$> currentOffset


-- | Increment base pointer offset by a multiplier
incrementOffsetByN :: Int -> GenState ()
incrementOffsetByN n = incOffset n


-- | Retrieve current value of offset
currentOffset :: GenState Int
currentOffset = do
        currFuncName <- FrameStack.currentFunc
        funcOffset <$> getFuncState currFuncName


incOffset :: Int -> GenState ()
incOffset n = do
        name  <- FrameStack.currentFunc
        fs    <- getFuncState name
        let fs' = fs { funcOffset = funcOffset fs + (n * memOffset) }
        setFuncState name fs'
