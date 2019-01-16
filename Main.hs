

import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)
import Control.Monad (when)
import qualified Data.Map as M

import Lexer (tokenize)
import Parser (parse)
import Generator (genASM)
import SymTab

--type SymTab = ()


newSymTab :: SymTab
newSymTab = Tab (-8) M.empty


main :: IO()
main = do
        --print $ parse $ tokenize "int main() { int a; return 5; }"
        print $ parse $ tokenize "int main() { int a = 5; return a; }"
        print $ parse $ tokenize "int main() { int a; a = 5; return a; }"
        --print $ parse $ tokenize "int main() { int a = 2 || 3; return 5; }"
        --print $ parse $ tokenize "int main() { int a = 2; int b = a + 1; return b; }"
        --print $ parse $ tokenize "int main() { int a = 2 * (7 + 3); return a; }"
        args <- getArgs
        let infileName = head args
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle

        -- debugging
        --print contents
        --print $ tokenize contents
        --print $ parse $ tokenize contents

        let outfileName = (dropExtension infileName) ++ ".s"
        let parsed = parse $ tokenize contents
        --let assembly = genASM $ parse $ tokenize contents

        let symTab = newSymTab --M.empty
            Ev act = genASM parsed
            (asm, symTab') = act symTab
            in do
                    --writeFile outfileName asm

                    when (length asm > 0) $
                       writeFile outfileName asm

        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
