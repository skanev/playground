# Week 16 (2012-12-04 - 2012-12-11)

## Meetups

We haven't met in ages. I'm venturing forward by myself. So far, it has been an interesting experience.

## Abstractions in Scheme

It becomes apparent to me, that builting a lot of small abstractions is really important in order to make scheme code readable. For example, exercise 4.49 requires generation instead of parsing of sentences. I modified the parse-word procedure in the following way:

    (define (parse-word word-list)
      (if (null? (cdr word-list))
          (amb)
          (amb (list (car word-list) (cadr word-list))
               (parse-word (cons (car word-list) (cddr word-list))))))

This obviously works, but it is far from optimal. The text defined the procedure an-element-of, which I could use to rewrite it as:

    (define (parse-word word-list)
		  (list (car word-list) (an-element-of (cdr word-list))))

The result is obviously way simpler. Instead, I went for recursion.

I'm not sure why I lack the discipline to introduce such small nice abstractions. On one hand, this follows naturally from the TDD process - you first beat the code until it works and only then you introduce abstractions. On the other hand, I'm rarely carrying this through - once I've gotten the tests of the exercises to pass, I continue with the next one.

There is a take-away on TDD, design and disciple here, that I've not yet formulated.
