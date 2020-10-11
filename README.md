# succ [![Build Status](https://travis-ci.com/jgthomas/succ.svg?branch=master)](https://travis-ci.com/jgthomas/succ) [![codecov](https://codecov.io/gh/jgthomas/succ/branch/master/graph/badge.svg)](https://codecov.io/gh/jgthomas/succ)

**S**uper **U**seless **C** **C**ompiler

* Multi-pass compiler
* Growing support for C language features
* Compiles C to x86-64 assembly
* Robust error handling system, with pretty printed error messages
* Basic type checking, supports int, int* and int[]

## testing

**Unit tests** are in this repo's *test* directory

**Functional tests** are in their [own repo](https://github.com/jgthomas/write_a_c_compiler)

## supported language elements

**Integer literals**
* Return statements (return 1)

**Unary operators**
* Sign (-, +)
* Logical (!)
* Bitwise (~)
* Prefix (++, --)
* Postfix (++, --)

**Binary operators**
* Additive (+, -)
* Multiplicative (*, /, %)
* Equality (==, !=)
* Relational (>, <, >=, <=)
* Logical (&&, ||)
* Bitwise (^, <<, >>, &, |)
* Assignment (=, +=, -=, *=, /=, %=, ^=, |=, &=, <<=, >>=)

**Local variables**
* Declaration (int a)
* Assignment (a = 10)
* Declaration and assignment (int a = 10)

**Conditionals**
* if
* else
* conditional expressions (a = 1 ? 2 : 3)

**Loops**
* for
* while
* do while
* break
* continue

**Multiple functions**
* Function calls
* Function declarations
* Function definitions
* Passing parameters using x86-64 calling convention

**Global variables**
* Declaration
* Definition
* Initialized in *.data*
* Uninitialized in *.bss*

**Pointers**
* Declared and defined locally or globally
* Pointing to local or global variables
* Passing as arguments
* Pass without assignment using '&'

**Rudimentary Type Checking**
* int vs. int*
* Check sides of assignments match
* Compare parameters to arguments
* Check function declaration against return values

## acknowledgements 

[Nora Sandler](https://norasandler.com/2017/11/29/Write-a-Compiler.html), who broke down compilers in a practical way.

[Bartosz Milewski](https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell/4-symbolic-calculator-recursion), who gave me a glimpse into how this might be done in Haskell.
