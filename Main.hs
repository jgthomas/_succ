
module Main (main) where


import System.IO
import System.Environment (getArgs)
import System.FilePath (dropExtension)
import System.Process (system)

import Lexer (tokenize)
import Parser (parse)
import Generator


main :: IO()
main = do
        print $ tokenize "int main() {return 1;}"
        print $ parse $ tokenize "int main() {return 1;}"
        print $ extractFrom $ parse $ tokenize "int main() {return 1;}"
        print $ tokenize "int main() {return -1;}"
        print $ parse $ tokenize "int main() {return -1;}"
        print $ extractFrom $ parse $ tokenize "int main() {return -1;}"

        args <- getArgs
        let infileName = head args
        let outfileName = (dropExtension infileName) ++ ".s"
        print infileName
        handle <- openFile infileName ReadMode
        contents <- hGetContents handle
        let tokens = tokenize contents
        print tokens
        let parsedTree = parse tokens
        print parsedTree
        let extractedNodes = extractFrom parsedTree
        print extractedNodes
        let outfileText = progString extractedNodes
        print outfileText
        writeFile outfileName outfileText
        system $ "gcc " ++ outfileName ++ " -o " ++ (dropExtension outfileName)
        system $ "rm " ++ outfileName
        hClose handle
