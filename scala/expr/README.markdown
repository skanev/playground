# Expression evaluator

A tiny interpreter for simple arithmetic expressions.

## Description

I wrote this project in order to learn Scala. I enjoy writing this interpreter
in different languages, because it has a few interesting learning
opportunities. It contains the following:

* AST structure for expressions (using case classes)
* A parser for expressions (using `scala.parser.combinators`)
* An interactive interpreter for arithmetic expressions
* Expressions can be evaluated concurrently (using `scala.actors`)
* Functions can be defined in terms of other expressions
* Functions can be defined in Scala

## Firing it up

It's as simple as:

    → sbt run
    Type 'help' for help.
    > forty_five_degrees = 0.25 * PI
    > tangent = lambda(x) { sin(x) / cos(x) }
    > tangent(forty_five_degrees)
    = 0.9999999999999999

It almost got it right. You can ask for `help` too:

    > help
    Usage instructions:
      * write any expression in order to evaluate it:
    
         1 + 2 + 3 + 5 + 7 + 11 + 13
         X + add(2, 4)
         1 + 2 - 3 * 4 / 5 ^ 6
    
      * assign variables or define functions with =
    
         ANSWER = 42
         add = lambda(X, Y) { X + Y }
    
      * other available commands
    
         names -- show all defined names
         exit  -- quit the interpreter
         help  -- you are looking at it

## How did it go?

While reading [Seven Languages in Seven Weeks][1], I realized I cannot reach a
good enough understanding of Scala just from the examples. Therefore, I read
[Programming in Scala][2] and went through some of the code. After that I
thought it will be fun to write a tiny interpreter, so I can experiment with
few features of Scala (pattern matching with extractors and case classes,
parser combinators and ScalaCheck, to name a few).

All went fine, until I attempted to evaluate expressions concurrently with
actors. It took me two days get it working and the activities included plowing
through the `scala.actors` documentation, purchasing a beta version of
[Actors in Scala][3], reading the papers ([first][4] and [second][5]) on the
Actor implementation and going through most of the code in `scala.actors`.
I finally got it right, but I'm not convinced that it is the best way to do it.

## Things I learned

In no specific order:

* Writing tests when learning a language is **very** good idea. I usually end
  up doing a lot of experiments and having feedback on a key press (as opposed
  to rerunning) is a huge time-saver.
* Furthermore, tweaking the environment to shave off a few seconds out of the
  common activities is priceless. I spent too much time typing stuff in sbt
  before I automated it in my Vim.
* Knowledge of the tools surrounding a language is as important as knowledge
  of the language itself. If I knew more about the Scala incremental compiler,
  ScalaTest and sbt, I would have been a lot more effective in learning the
  language. I should take the time to study to whole toolchain involved.
* Vim is not the best fit for a complex, static typed language like Scala. I
  was craving for IDE features. Seeing the type of an expression or the
  available methods would be priceless.
* Applying an incremental approach to writing code in a new language pays off.
  I've spent most of the time refactoring code I've gotten to work. It is a
  great learning opportinuty, since it allows finding out small tricks for
  DRYing up code in the safety net of tests.

  [1]: http://pragprog.com/titles/btlang/seven-languages-in-seven-weeks
  [2]: http://www.artima.com/shop/programming_in_scala
  [3]: http://www.artima.com/shop/actors_in_scala
  [4]: http://lampwww.epfl.ch/~odersky/papers/jmlc06.pdf
  [5]: http://lamp.epfl.ch/~phaller/doc/haller07coord.pdf
