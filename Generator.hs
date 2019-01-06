
module Generator (generate) where


import Parser


generate :: Tree -> String
generate x = ".globl main\nmain:\nmovq $5, %rax\nret\n"
