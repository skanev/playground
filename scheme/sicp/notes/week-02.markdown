# Week 2 (2012-03-06 - 2012-03-13)

This is my second week in the book. I'm still very excited, but I am also gotten the farthest in the exercise from all the people in the study group. Thus, I will attempt to take it slower. It will not be that hard, since I have travelling involved.

## Questions

* What is the time complexity of 1.14?
* What is the time complexity of normal-order in 1.20?
* Am I getting the Robin-Miller test from 1.28 wrong? I expected all numbers a < n to fail the simple test (the one without the non-trivial square root test).
* In 1.29, my implementation of Simpson's rule appears to be less precise. Am I doing something wrong?

## Various

### Uncle Bob's method ordering convention

I'm following Uncle Bob's approach to ordering procedures in Scheme. I am putting the most important function on top. It will refer undefined (yet) procedures. I find the first such procedure in the body and put it afterwards. If the second procedure has undefined names, I define them before continuing with the remaining undefined names from the first procedures. Essentially, this orders the procedures depth-first.

The argument is that this way the code reads from top to bottom. I don't find it optimal, but I believe it works great in this case.

### Recursive process vs. recursive procedure

This is from chapter 1.2.1, and while I've known it all along, it took me a while to appreciate the subtlety. Consider a tail-recursive and a non-tail-recursive implementations of factorial. Both of them are recursive procedures. But the first generates an iterative process, while the latter - a recursive one.

It's curious that recursive definitions do not always imply recursive processes. As far as I can tell, this totally depends on the tail recursion call.

Which got me thinking about tail recursion. While it's extremely nice, there is the risk of modifying a tail-recursive function in a way that preserves correctness, but makes it non-tail-recursive. This can happen accidentally and can be a regression, because the function will run out of stack space for larger inputs. That's why I find it extremely nice that Scala has a `@tailrec` annotation that enforces tail recursion.

## Meetup summary

It was rather quick. I found out that some of my thoughts were utterly invalid and that some of my solutions were wrong. Here are some assorted thouhgts:

* Plamen claims that it is proven that n.log(n) is the fastest a sorting algorithm can get. I'm not so certain about this, but hey - what do I know about sorting.
* I should check out what Radix sort is. It is purported to be constrained, but faster.
* My note from Week 1 on tail recursion + normal-order evaluation needs some clarification. I claimed that the time gets worse when both are present. It is actually not true, since normal-order evaluation is sufficient to degrade performance. Tail recursion is a factor, however, in the way you write programs. I would claim that if you had no tail recursion in your language, you would not go with a recursive procedure. Therefore, it is very unlikely that you stumble in that kind of degradation. Normal-order would still be slower, however.
* I got 1.15 wrong. Time is Θ(logn).
* I sill have questions from earlier. I will not carry them over to Chapter 2, but I will still keep them around in hope that somebody would provide a satisfactory answer.
