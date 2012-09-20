# Structure and Interpretation of Computer Programs

I'm doing a SICP study group with a couple of friends. This is where I will kepp the solutions to the exercises.

## Scheme implementation

Despite this being SICP, I've chosen to use [Racket](http://racket-lang.org/).  The people there have their own opinion on how to do an introductionary course in Scheme, called [How to Design Programs](http://www.htdp.org/). This is beside the point, though - the idea here is to read through SICP, not to learn Scheme or programming.

At the time, Racket does not have a decent sicp language pack. Instead of trying to figure out a way to restrict the language, I will try to stick at the R5S5 subset, using Racket. If that fails for some reason, I might reconsider.

## Tests

I like to keep a bunch of tests for each exercise. I'm just that kind of person.

Although I will do the solutions in SICP-level Scheme, I will not hesitate to use full-blown Racket in the tests. This will give me a nice opportunity to do some cargo-culting, which might be useful if I want to learn Racket in the future.

## Tools

I'm not just interested in solving the exercise. I'm interested in building an environment that is up for the task, tailored to me. If Racket was a bit more modern, I would have tried to go with its own tools. Since this is not the case (IMHO), I will just use Ruby.

### Generating scaffolds

It is fairly simple. If you want to generate a scaffold for exercise 1.01, just run:

    rake exercise[1,1]

If you want to generate the scaffold for the next unsolved task, just do:

    rake next

### Running tests

Tests can be run the following way:

    rake run:exercise[1,1]

If you want to run all tests do:

    rake run:all

You can also use watchr to do continuous testing. Just run:

    rake watch

## Showcases

Some of the chapters require chunks of work that don't fit as exercises. Instead, I need a larger program that can show something meaningful. Occasionally, I will create a "showcase" for those. You can see a list of showcases by running `rake -T`.

The showcases have a specific directory structure, that's pretty easy to grok. Note, that if you want to run a showcase with a different version of Racket, you need to specify the `RACKET` environment variable to point to an executable. For example:

    RACKET=/path/to/racket rake run:showcase:example

### The Picture Language (section 2.2.4)

This is the Escher-like picture library, introduced when talking about data abstraction and robust design. Note, that this might not run with the Racket that you install from Homebrew. It has a bunch of problems with depending on `libcairo` that I don't care to debug. Downloading a pre-compiled distribution from the Racket site is enough.

    RACKET=~/Code/runtime/racket/bin/racket rake run:showcase:picturelang

### The metacircular evaluator (section 4.1)

This is the basic metacircular evaluator explored in Chapter 4 for the book. It is very basic and featureless. It is fairly easy to run it, although you should be careful what you are typing in. Run it with:

    rake run:showcase:evaluator
