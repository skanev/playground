# Week 2 (2012-03-06 - 2012-03-13)

This is my second week in the book. I'm still very excited, but I am also gotten the farthest in the exercise from all the people in the study group. Thus, I will attempt to take it slower. It will not be that hard, since I have travelling involved.

## Meetup summary

We established that we the target for next week is 1.3.2. One of my question was answered, but the other two remain a mystery to me.

### Notes
* Is [Nikolay's `good-enough?`][nb-01-07] different?
  * Nikolay says that he is doing multiplication, while I'm doing divisions. He claims that multiplication is (1) faster and (2) more accurate. He does not quote sources.
* Ackermann's function seems pretty useless to me, but apparently it has some interesting properties. It is mostly theoretical, though.

[nb-01-07]: https://github.com/nb/sicp/blob/a468e7e08c03cde42b317d94b5cf0e4db7613212/1.7.scm

## Questions

* What is the time complexity of 1.14?
* What is the time complexity of normal-order in 1.20?
* Am I getting the Robin-Miller test from 1.28 wrong? I expected all numbers a < n to fail the simple test (the one without the non-trivial square root test).

## Various

### Uncle Bob's method ordering convention

I'm following Uncle Bob's approach to ordering procedures in Scheme. I am putting the most important function on top. It will refer undefined (yet) procedures. I find the first such procedure in the body and put it afterwards. If the second procedure has undefined names, I define them before continuing with the remaining undefined names from the first procedures. Essentially, this orders the procedures depth-first.

The argument is that this way the code reads from top to bottom. I don't find it optimal, but I believe it works great in this case.
