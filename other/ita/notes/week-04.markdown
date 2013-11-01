# Week 04 (2013-10-16 - 2013-10-23)

## On week numbers

We were very bad on keeping it regular in the summer. I had some
remarks about chapter 4, but I am going to skip them and just
jump ahead in my notes.

## Various remarks

* I had to solve exercise C.4-9 before having an idea about
  solving C.4-8. I spent an hour on the later. At least I learned
  about [coupling][coupling]
* Exercise C.1.9 gives an interesting interpretation of the
  second diagonal of Pascal's triangle
* I'm starting to reflect on how much of mathematics is not
  intuition about logical laws, but symbol manipulation. It is a
  very interesting distinction.
* I was mindblown by the sum used to prove exercise C.2.6

[coupling]: http://en.wikipedia.org/wiki/Coupling_%28probability%29

## Doing it twice

For Appendix C, I first solved everything with pen and paper and then went
through the long and tedious process of entering everything into Markdown and
LaTeX. The second part was not so much fun, but I got to revisit the exercises
a couple of days after I did them, which helper reinforce my knowledge.

Revisiting what you learned is important.

## Probability

I'm having tons of fun doing probability. Especially things like
verifying the results of exercise C.4-2 with the following Ruby
script:

    n = 1_000_000
    def try
      [1].cycle.take_while { rand(2**6).to_s(2).count("1") != 3 }.count + 1
    end

    p n.times.map { try }.inject(:+) / n.to_f

And surprise - the results match!
