

import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)
import Control.Monad (when)

import Lexer (tokenize)
import Parser (parse)
import Generator (genASM)


main :: IO()
main = do
        print $ parse $ tokenize "int main() { int a; return 4; }"
        print $ parse $ tokenize "int main() { int a = 5; return 4; }"
        print $ parse $ tokenize "int main() { int a; a = 5; return 4; }"
        print $ parse $ tokenize "int main() { int a = 2 || 3; return 5; }"
        print $ parse $ tokenize "int main() { int a = 2; int b = a + 1; return b; }"
        args <- getArgs
        let infileName = head args
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle

        -- debugging
        print contents
        print $ tokenize contents
        print $ parse $ tokenize contents

        let outfileName = (dropExtension infileName) ++ ".s"
        let assembly = genASM $ parse $ tokenize contents

        when (length assembly > 0) $
           writeFile outfileName assembly

        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
