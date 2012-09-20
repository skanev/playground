# Week 15 (2012-09-18 - 2012-09-25)

## Meetups

We have been very slow on doing our meetups. On the other hand, I've been quite quick in progressing through the book. Currently I'm running far infront of everybody, since the book is finally starting to get interesting.

## On namespaces and objects

This is not related to SICP, but an interesting thought nontheless. Once you get back to procedural/functional programming, you get into a nightmare of managing names in each namespace. If you are using a bunch of modules, you need to import the names you need and make sure they are available in a maintainable way. Think about `from foo import bar as baz` in Python or the imports in Scala.

Object-oriented programming gets that out of the way, since it namespaces all operations under the type. This is obviosly limiting (for example, no "global" functions in Java), but makes name handling a lot easier - you just import the class and Robert's your uncle.
