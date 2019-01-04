module Main (main) where


import Lexer (tokenize)


main :: IO()
main = do
        print $ tokenize "int main(){return 100;}"
