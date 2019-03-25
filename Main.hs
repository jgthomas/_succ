

import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)
import Control.Monad (when)

import Lexer (tokenize)
import Parser (parse)
import Generator (genASM)
import SymTab (Evaluator(..), newSymTab)


main :: IO()
main = do
        --print $ tokenize "int dog(int x) { return 5; } int main() { return dog(6); }"
        print $ parse $ tokenize "int main() { dog(1); return 2; }"
        print $ parse $ tokenize "int dog(int x) { return 5; } int main() { return dog(6); }"
        --print $ parse $ tokenize "int dog(int x) { return x; } int main() { return 2; }"
        --print $ parse $ tokenize "int main() { dog(cat(mouse(), 12), 2, a); return 2; }"
        args <- getArgs
        let infileName = head args
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle

        -- debugging
        --print contents
        --print $ tokenize contents
        print $ parse $ tokenize contents

        let outfileName = (dropExtension infileName) ++ ".s"
        let parsed = parse $ tokenize contents

        let symTab = newSymTab
            Ev act = genASM parsed
            (asm, symTab') = act symTab
            in do
                    print symTab'
                    when (length asm > 0) $
                       writeFile outfileName asm

        system $ "gcc -g " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
