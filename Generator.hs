
module Generator (generate) where


import Parser


generate :: Tree -> String
generate x = ".globl _start\n_start:\nmovq $1, %rax\nret"
