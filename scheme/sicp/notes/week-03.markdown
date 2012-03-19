# Week 3 (2012-03-13 - 2012-03-20)

This is the third week of our humble study group. Some of us are lagging behind, but nontheless, we decided that we will complete Chapter 1.

## Meetup summary

It was rather quick. I found out that some of my thoughts were utterly invalid and that some of my solutions were wrong. Here are some assorted thouhgts:

* Plamen claims that it is proven that n.log(n) is the fastest a sorting algorithm can get. I'm not so certain about this, but hey - what do I know about sorting.
* I should check out what Radix sort is. It is purported to be constrained, but faster.
* My note from Week 1 on tail recursion + normal-order evaluation needs some clarification. I claimed that the time gets worse when both are present. It is actually not true, since normal-order evaluation is sufficient to degrade performance. Tail recursion is a factor, however, in the way you write programs. I would claim that if you had no tail recursion in your language, you would not go with a recursive procedure. Therefore, it is very unlikely that you stumble in that kind of degradation. Normal-order would still be slower, however.
* I got 1.15 wrong. Time is Θ(logn).
* I sill have questions from earlier. I will not carry them over to Chapter 2, but I will still keep them around in hope that somebody would provide a satisfactory answer.

## Questions

* What is the time complexity of 1.14?
* What is the time complexity of normal-order in 1.20?
* Am I getting the Robin-Miller test from 1.28 wrong? I expected all numbers a < n to fail the simple test (the one without the non-trivial square root test).
* In 1.29, my implementation of Simpson's rule appears to be less precise. Am I doing something wrong?

## Ruby, lambdas and procedures as general methods

Chapter 1.3.3 makes a very nice point about procedures. When a procedure is parametrized with functions (as opposed to values), it can create a more powerful abstraction. The `fixed-point` function is a nice example of that - it is a general abstraction for Newton's method of finding square roots (exercise 1.07). This is cool, but I would say that it is not used in Ruby.

Ruby is pretty decent, when your method needs just one lambda - it can be passed as a block. When you need too lambdas, however, it becomes rather awkward. Ruby has the `lambda` keyword, but it has at least two problems - (1) mullti-ine lambdas are tricky (you have to use `{`/`}` instead of `do`/`end`) and (2) it feels awkward to pass lambdas to functions an invoke them. In such cases, I would probably go with a Template Method instead.

While Ruby has all the power and flexibility to do this, I feel that the Ruby culture frowns upon it. Thus, Ruby feels less powerful in that sense. Note that it just **feels** that way.

## First-class citizens and Ruby

This is a continuation of the previous thought. Section 1.3.4 talks about procedures as first-class citizens and what the term means. It says, that when more elements are first-class, this provides for more powerful abstractions.

It is curious how this question looks in Ruby. Are methods first-class in Ruby or not? On one hand, they are, since they (1) may be named as variables, (2) may be passed as arguments to procedures, (3) can be returned as the result of procedures and (4) may be included in data structures. Working with them, however, is slightly awkward, since they are obtained with `#method` and need special handling (invoking with `#call`). The awkwardness is very apparent when you compare with Python.

I find the following interesting: the slight awkwardness in Ruby makes Ruby programmers avoid those constructs. Nobody really ever passes a method or returns a lambda to be called. I find two reasons for this. First, the code appears un-Rubyic, which is a cultural reason. Second, and more to the point, this is not orthogonal to blocks - most of those things can be accomplished by blocks, which makes such use of the constructs unpopular.

Bottom line, I find the concept of "first-class citizens" blurry at best. It might be useful to explain the differences of procedures in C and those in LISP, but it is very imprecise in Ruby. Methods there appear to be first-class in letter, but not in spirit. Which is a useful insight on its own.
