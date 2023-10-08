# haskell-brainfuck

## Installation
1) clone this repository to a location of your choice
2) cd into the directory
3) run `cabal install`

## Usage
There are three main ways to use the interpreter, namely reading from a file, taking code as an argument directly and reading from standard input.
```
>>> brainfuck file hello-word.bf
hello, world!

>>> brainfuck string "--[----->+<]>----.[--->+<]>----.+++[->+++<]>++.++++++++.+++++.--------.-[--->+<]>--.+[->+++<]>+.++++++++."
brainfuck

>>> echo "+++++++[>++++++<-]>." | brainfuck pipe
*
```
