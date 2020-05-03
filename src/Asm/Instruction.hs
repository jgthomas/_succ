
module Asm.Instruction where


data Set = Equ
         | NEqu
         | GThan
         | GThanE
         | LThan
         | LThanE
         deriving (Eq)


data Jump = JMP
          | JE
          | JNE
          deriving (Eq)


setGotoPoint :: Int -> String
setGotoPoint target = emitJump JMP target


add :: String -> String -> String
add a b = "addq " ++ a ++ ", " ++ b ++ "\n"


imul :: String -> String -> String
imul a b = "imul " ++ a ++ ", " ++ b ++ "\n"


sub :: String -> String -> String
sub a b = "subq " ++ a ++ ", " ++ b ++ "\n"


idivq :: String -> String
idivq target = signExtendRaxRdx ++ "idivq " ++ target ++ "\n"


inc :: String -> String
inc a = "inc " ++ a ++ "\n"


dec :: String -> String
dec a = "dec " ++ a ++ "\n"


xorBits :: String -> String -> String
xorBits a b = "xor " ++ a ++ ", " ++ b ++ "\n"


andBits :: String -> String -> String
andBits a b = "and " ++ a ++ ", " ++ b ++ "\n"


orBits :: String -> String -> String
orBits a b = "or " ++ a ++ ", " ++ b ++ "\n"


shiftBitsLeft :: String -> String -> String
shiftBitsLeft n dest = "sal " ++ "$" ++ n ++ ", " ++ dest ++ "\n"


shiftBitsRight :: String -> String -> String
shiftBitsRight n dest = "sar " ++ "$" ++ n ++ ", " ++ dest ++ "\n"


push :: String -> String
push s = "pushq " ++ s ++ "\n"


pop :: String -> String
pop s = "popq " ++ s ++ "\n"


call :: String -> String
call f = "call " ++ f ++ "\n"


move :: String -> String -> String
move s d = "movq " ++ s ++ ", " ++ d ++ "\n"


loadAddOf :: String -> String -> String
loadAddOf s d = "leaq " ++ s ++ ", " ++ d ++ "\n"


comp :: String -> String -> String
comp a b = "cmpq " ++ a ++ ", " ++ b ++ "\n"


makeNegative :: String -> String
makeNegative s = "neg " ++ s ++ "\n"


invertBits :: String -> String
invertBits s = "not " ++ s ++ "\n"


returnControl :: String
returnControl = "ret\n"


setBitIf :: Set -> String
setBitIf set =
        case set of
             Equ    -> "sete " ++ bit ++ "\n"
             NEqu   -> "setne " ++ bit ++ "\n"
             GThan  -> "setg " ++ bit ++ "\n"
             GThanE -> "setge " ++ bit ++ "\n"
             LThan  -> "setl " ++ bit ++ "\n"
             LThanE -> "setle " ++ bit ++ "\n"
        where bit = "%al"


signExtendRaxRdx :: String
signExtendRaxRdx = "cqto\n"


emitJump :: Jump -> Int -> String
emitJump j n =
        case j of
             JMP -> "jmp _label_" ++ num ++ "\n"
             JE  -> "je _label_" ++ num ++ "\n"
             JNE -> "jne _label_" ++ num ++ "\n"
        where num = show n


literal :: Int -> String
literal n = "$" ++ show n
