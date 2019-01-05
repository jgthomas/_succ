
module Main (main) where


import Lexer (tokenize)
import Parser (parse)


main :: IO()
main = do
        print $ tokenize "int main(){return 100;}"
        print $ parse $ tokenize "100"
        print $ parse $ tokenize "return 100"
        print $ parse $ tokenize "int main return 100"
