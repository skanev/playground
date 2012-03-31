# Week 4 (2012-03-20 - 2012-03-27)

We are finally starting Chapter 2. It goes into data structures and I'm super excited. Plus, I really, really, REALLY like using `car` and `cdr` for recreation.

## Questions

* There is a quote in the foreword - "In Pascal the plethora of declarable data structures induces a specialization within functions that inhibits and penalizes casual cooperation. It is better to have 100 functions operate on one data structure than to have 10 functions operate on 10 data structure.". I'm not sure what it means, because of the ambiguity - "It is better in Pascal" or "It is better in general"?
* How do you put more than one call in an if branch? (2.23)
* Can 2.28 be done with a simple recursion, without reverse and append?

## Various

* With the rational numbers example, calculating the `gcd` can be done either in construction time (`make-rat`) or in selection time (`numer` and `denom`). This is a good example of the flexibility in data abstraction. I didn't occur to me that doing it in selection time can be faster in some cases.
* The parts of subtraction are called "minuend", "subtrahend" and "difference". Neat.
* "The names car and cdr persist because simple combinations like cadr are pronounceable." This even sounds true. Wow.

## Constructors, selectors and abstraction barriers

There is an interesting difference in how SICP advocates designing compound data and classical OOP wisdom. It is easy to show when implementing rational numbers. In an object-oriented fashion, we will have two levels of abstraction - (1) the clients of the `Rational` class and the (2) implementation of its operations (as methods). The methods use the private representation of the class. In contrast, the book talks about three levels - (1) the clients that use rational numbers, (2) the rational number operations (addition, subtraction, multiplication, divison) and (3) a set of constructors and selectors, what the operations are implemented with. That way representation can change without affecting the operations.

If we have a class `Rational`, the operations (2nd layer) and the internal representation (3rd layer) are naturally grouped in the same context (the class). Usually there is no separation - the operations access the private state directly. This makes changing the representation harder.

Introducing a layer of _constructors and selectors_ in a Ruby class seems a neat idea. I wonder if there is a good way to make it explicit.

## Procedural representation of pairs

I know what procedural representation of data is, but this caught me off guard - you can implement LISP pairs with lambdas. Like this:

    (define (cons x y)
      (define (dispatch m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (else (error "Argument not 0 or 1 -- CONS" m))))
      dispatch)

    (define (car z) (z 0))
    (define (car z) (z 1))

While I never gave serious thought about it, I always assumed pairs have to be implemented in C. Interestingly, this will be a sufficient implementation for a lot of LISP code.

I'm curious how `pair?` will be implemented in this approach.

## Mathematics and naïveté

The extended exercise of 2.1.4 shows an interesting insight: you can rarely approch mathematics naïvely. The idea was generally sound in the beginning, but the accumulated error in tolerance quickly makes the code untrustworthy. Furthermore, in order to minimize the error, one has to devise a way of simplifying arithmetic expressions. This is a task, way larger than the original.

Another interesting aspect of the exercise, is that the accumulated error was not immediatelly obvious from the initial design.

## Meetup summary

This time I was in London and we held a brief meeting over Skype. Everybody was in the beginning of the material, except for me and Plamen.

* Plamen seems to enjoy the video lectures. I should check them out.
* I should show the real-time aspect of how I implement the exercises.
* Veselin said he will show us the group annotation software next time.

We decided to keep the scope to 2.2.2, although Plamen and I might race a bit forward.
