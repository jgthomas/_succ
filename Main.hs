
module Main (main) where


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
        --print $ tokenize "int main() { return 1 + 1;}"
        --print $ parse $ tokenize "int main() { return 1 + 1;}"
        --print $ tokenize "int main() { return (1 + 1) + 2;}"
        --print $ parse $ tokenize "int main() { return (1 + 1) + 2;}"
        --print $ tokenize "int main() { return 2 - 1 + 2;}"
        --print $ parse $ tokenize "int main() { return 2 - 1 + 2;}"
        --print $ parse $ tokenize "int main() { return 2 * 1 / 2;}"
        --print $ parse $ tokenize "int main() { return 2 - 1 - 2;}"
        --print $ tokenize "int main() { return 1 * 2;}"
        --print $ parse $ tokenize "int main() { return 1 * 2;}"
        --print $ genASM $ parse $ tokenize "int main() {return 1 + 2;}"
        --print $ genASM $ parse $ tokenize "int main() {return 2 / 2;}"

        --print $ parse $ tokenize "int main() {return 6 / 3 / 2;}"
        --print $ parse $ tokenize "int main() {return 6 * 3 / 2;}"
        --print $ parse $ tokenize "int main() {return 3 - 3 + 3;}"
        --print $ parse $ tokenize "int main() {return 3 / 3 * 3;}"
        --print $ parse $ tokenize "int main() {return 6 - 3 + 2;}"
        --print $ parse $ tokenize "int main() {return 6 + 3 * 2;}"

        args <- getArgs
        let infileName = head args
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle

        -- debugging
        print $ tokenize contents
        print $ parse $ tokenize contents

        let outfileName = (dropExtension infileName) ++ ".s"
        let assembly = genASM $ parse $ tokenize contents

        when (length assembly > 0) $
           writeFile outfileName assembly

        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
