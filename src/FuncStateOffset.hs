
module FuncStateOffset
        (currentOffset,
         incrementOffsetByN,
         stackPointerValue
        ) where


import qualified FrameStack      (currentFunc)
import           FuncStateAccess (getFuncState, setFuncState)
import           GenState        (GenState)
import           SymbolTable     (FuncState (funcOffset), memOffset)


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
