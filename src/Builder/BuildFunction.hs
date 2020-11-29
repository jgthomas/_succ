module Builder.BuildFunction
  ( funcPrologue,
    funcEpilogue,
    functionCall,
  )
where

import Builder.Directive (declareGlobl, globlLabel)
import Builder.Instruction (call, move, pop, push, returnControl)
import Builder.Register
  ( Register (..),
    allScratch,
    params,
    reg,
    selectRegister,
  )

funcPrologue :: String -> String
funcPrologue funcName =
  declareGlobl funcName
    <> globlLabel funcName
    <> runInit funcName
    <> saveBasePointer
    <> saveRegisters allScratch

runInit :: String -> String
runInit "main" = "jmp init\n" <> "init_done:\n"
runInit _ = ""

funcEpilogue :: String
funcEpilogue =
  restoreRegisters allScratch
    <> restoreBasePointer
    <> returnControl

functionCall :: String -> [(String, Int)] -> String
functionCall name args =
  saveCallerRegisters
    <> concatMap passArgument args
    <> call name
    <> restoreCallerRegisters

passArgument :: (String, Int) -> String
passArgument (argAsm, pos) = argAsm <> putInRegister pos

putInRegister :: Int -> String
putInRegister r = move (reg RAX) (selectRegister r)

saveCallerRegisters :: String
saveCallerRegisters = saveRegisters params

restoreCallerRegisters :: String
restoreCallerRegisters = restoreRegisters params

saveRegisters :: [Register] -> String
saveRegisters rs = concatMap (push . reg) rs

restoreRegisters :: [Register] -> String
restoreRegisters rs = concatMap pop . reverse . fmap reg $ rs

saveBasePointer :: String
saveBasePointer =
  push (reg RBP)
    <> move (reg RSP) (reg RBP)

restoreBasePointer :: String
restoreBasePointer =
  move (reg RBP) (reg RSP)
    <> pop (reg RBP)
