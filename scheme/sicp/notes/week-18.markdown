# Week 18 (2012-12-18 - 2012-12-25)

## Notes

This week is about the first four sections of the final chapter - everything but compilation.

## Recursive vs. Iterative

Exercuse 5.27 shows some interesting results. The recursive factorial is not bound in the stack size it consumes, but it performs the computation with less pushes. I'm curious whether this is a weirdness of the explicit control evaluator, or whether it is actually true in general. The idea that recursive functions can be faster than iterative in some cases is interesting. I did not spend much time researching, but there is an interesting [answer on Stack Overflow][answer].

[answer]: http://stackoverflow.com/questions/2651112/is-recursion-ever-faster-than-looping/2651200#2651200
