# succ

**S**uper **U**seless **C** **C**ompiler

Compiling a laughably small subset of C to unoptimised assembly since 2019

## full credit to my teachers

[Nora Sandler](https://norasandler.com/2017/11/29/Write-a-Compiler.html), who broke down compilers in a practical way.

[Bartosz Milewski](https://www.schoolofhaskell.com/user/bartosz/basics-of-haskell/4-symbolic-calculator-recursion), who gave me a glimpse into how this might be done in Haskell.

## supported language elements

0. **Integer literals**
* Return statements (return 1)

1. **Unary operators**
* Negation (-)
* Logical negation (!)
* Bitwise complement (~)

2. **Binary operators**
* Addition (+)
* Subtraction (-)
* Multiplication (*)
* Division (/)
* Modulo (%)
* Equal (==)
* Not equal (!=)
* Greater than (>)
* Less than (<)
* Greater than or equal (>=)
* Less than or equal (<=)
* Logical or (||)
* Logical and (&&)

3. **Local variables**
* Declaration (int a)
* Assignment (a = 10)
* Declaration and assignment (int a = 10)

4. **Conditionals**
* if
* else
* conditional expressions (a = 1 ? 2 : 3)

5. **Loops**
* for
* while
* do while
* break
* continue

## in progress

* Remaning operators
* Function calls
