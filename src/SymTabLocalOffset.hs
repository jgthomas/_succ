
module SymTabLocalOffset
        (currentOffset,
         incrementOffsetByN,
         stackPointerValue
        ) where


import qualified FrameStack        (currentFunc)
import           GenState          (GenState)
import           GenStateLocal     (FuncState (funcOffset))
import qualified GenStateLocal     (memOffset)
import           SymTabLocalShared (getFuncState, setFuncState)


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
        let fs' = fs { funcOffset = funcOffset fs + (n * GenStateLocal.memOffset) }
        setFuncState name fs'
