# Week 1 (2012-02-27 - 2012-03-06)

Notes on my first week with SICP. We have just started the study group.

## Questions

* Is [Nikolay's `good-enough?`][nb-01-07] different?
* What is the time complexity of 1.14?
* What is the time complexity of normal-order in 1.20?

[nb-01-07]: https://github.com/nb/sicp/blob/a468e7e08c03cde42b317d94b5cf0e4db7613212/1.7.scm

## Various

* I enjoy writing text on the side of the code. I don't do that in my day job, since I consider it an anti-pattern. It makes me more reflective.
* I enjoy reaching a certain depth in my fiddling with Scheme. It makes the experience nicer.
* I take a lot of time to ensure nice commit message and nice looking solutions. That way the code should be a pleasure to read.
* Consider giving me the thumbs up, if you are following along and enjoying it ;)
* Building tools is interesting, although it turned into yak shaving - I spent and hour and something on the Rakefile.
* Fib(n) can be calculated in o(log(n)). I knew it, but I still find it freaky.

## How to Design Programs

The Racket community has its own introductory-level book that teaches Scheme (called [How to Design Programs](http://htdp.org/). It takes a different approach from SICP and even criticizes it a bit. It looks promising - it is using DrRacket a lot and it even combines pictures and code in a cool way.

The book is even more basic than SICP, which doesn't make it very interesting to me. Besides, the point of the study group is to finish SICP, not HtDP. I might take a look at it once we're finished.

## Concepts, Techniques and Models of Computer Programming

There is another book that looks promising - [Concepts, Techniques and Models of Computer Programming][ctm]. It is dubbed by some people as "the book to read after SICP". It looks promising and there are some [nice comments about it on c2][ctm-on-c2]. I learned about it form a [thread on Hacker News][sicp-vs-htdp-hn] that is also worth taking a look at.

[ctm]: http://mitpress.mit.edu/catalog/item/default.asp?ttype=2&tid=10142
[ctm-on-c2]: http://c2.com/cgi/wiki?ConceptsTechniquesAndModelsOfComputerProgramming
[sicp-vs-htdp-hn]: http://news.ycombinator.com/item?id=428651

## Why MIT switched from Scheme to Python?

Sussman has an [interesting comment][why-python] on why MIT switched to Python. It is short and I cannot pick up a representative quote. To sum it up, programming has changed fundamentally and while in the past you could understand your program all the way down, now you have to do basic science on some of the libraries you are using. Definitelly worth reading it.

[why-python]: http://www.wisdomandwonder.com/link/2110/why-mit-switched-from-scheme-to-python

## Invariant quantity

There is an interesting idea in exercise 1.16. We're designing an iterative algoright to do fast exponentiation. For each iteration we are keeping three state variables (a, b and n) and we require that an invariant holds on each iteration (namely abⁿ is constant). The three of them change on each iteration, but the abⁿ remains unchanged. n is reduced on every iteration and when it finaly reaches 0, the result is in a.

Invariant quantities appear to be an interesting way to design algorithms.

## Tail recursion and normal-order evaluation

Normal-order evaluation can dramatically change the order of growth when combined with tail recursion. I thought it just changes the constant, but it can actually change the function.

Exercise 1.20 shows a nice example. In applicative-order evaluation, gcd has log(n) order of growth, but in normal-order evaluation it becomes a lot slower. I'm not sure what the new order of growth is, but it is at least linear.

I doubt it happens in practice, but it is still an interesting observation.

## Some school-level algebra

I just love it when I discover something neat in algebra I learned in school. Namely, there is a neat way to geometrically explain (n + 1)². Assume we have a square with size n and we want to increase its size with n. Here's a nice ASCII graphic:

    +---+---+---+---+---+---+ . +      Each "small square" has area of 1. The "thick" squares
    |   |   |   |   |   |   | x .      make up the original square with size n, while the
    +---+---+---+---+---+---+ . +      "thin" ones make up the extra areas we need to add
    |   |   |   |   |   |   | x .      to increase the size with 1. Let's count the added
    +---+---+---+---+---+---+ . +      squares. We add n squares on the right (marked with x)
    |   |   |   |   |   |   | x .      and another n squares on the bottom (marked with x).
    +---+---+---+---+---+---+ . +      We need to add an extra square, diagonally to complete
    |   |   |   |   |   |   | x .      the shape, marked with o. Thus, we end up adding 2n + 1
    +---+---+---+---+---+---+ . +      squares. When we take all together, we get:
    |   |   |   |   |   |   | x .
    +---+---+---+---+---+---+ . +      n² + 2n + 1
    |   |   |   |   |   |   | x .
    +---+---+---+---+---+---+ . +      which of course is:
    . x . x . x . x . x . x . o .
    + - + - + - + - + - + - + . +      (n + 1)²

It's easy enough to generalize this to (a + b)². The number of times we add a to each side is b, which makes it 2ab for the x squares. We need to add b² to complete the o squares. It ends up as:

a² + 2ab + b²

There is probably some Greek person that wrote this down a couple of thousand years ago, but somehow I managed to miss it. Anyway, it's still fun when I discover something simple about school math that I did not know.

## Meetup summary

We established that we the target for next week is 1.3.2. One of my question was answered, but the other two remain a mystery to me.

### Notes
* Is [Nikolay's `good-enough?`][nb-01-07] different?
  * Nikolay says that he is doing multiplication, while I'm doing divisions. He claims that multiplication is (1) faster and (2) more accurate. He does not quote sources.
* Ackermann's function seems pretty useless to me, but apparently it has some interesting properties. It is mostly theoretical, though.

[nb-01-07]: https://github.com/nb/sicp/blob/a468e7e08c03cde42b317d94b5cf0e4db7613212/1.7.scm
