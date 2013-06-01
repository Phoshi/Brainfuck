Brainfuck.hs
============

A brainfuck interpreter written in Haskell. It complies with the standard syntax and behaviour and is capable of running arbitary input. The code to run is given either as a filename or during invocation. Input to the code (consumed when the , command is used) is given prior to execution.

It is not a very fast interpreter, taking almost an entire second to run the [99 bottles of beer](http://www.99-bottles-of-beer.net/) brainfuck implementation when compiled with optimisation under ghc 7.4.2.