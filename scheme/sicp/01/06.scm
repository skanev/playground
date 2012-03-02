; SICP exercise 1.06
;
; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
; "Why can't I just define it as an ordinary procedure in terms of cond?" she
; asks. Alyssa's friend Eva Lu Ator claims this can indeed be done, and she
; defines a new version of if:
;
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))
;
; Eva demonstrates the program for Alyssa:
;
; (new-if (= 2 3) 0 5)
; 5
;
; (new-if (= 1 1) 0 5)
; 0
;
; Delighted, Alyssa uses new-if to rewrite the square-root program:
;
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))
;
; What happens when Alyssa attempts to use this to compute square roots?
; Explain.

; The interpreter gets stuck in an infinite loop.
;
; There is a crucial difference between if and new-if - the former does not
; evaluate both of its clauses, while the later does. This means, that despite
; whether the guess is good enough, sqrt-iter will call itself with an improved
; guess, ending up in an infinite recursion.
;
; Suffice to say, Eva was wrong.
