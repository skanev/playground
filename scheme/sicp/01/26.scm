; SICP exercise 1.26
;
; Louis Reasoner is having great difficulty doing exercise 1.24. His
; fast-prime?  test seems to run more slowly than his prime? test. Louis calls
; his friend Eva Lu Ator over to help. When they examine Louis's code, they
; find that he has rewritten the expmod procedure to use an explicit
; multiplication, rather than calling square:
;
; (define (expmod base exp m)
;   (cond ((= exp 0) 1)
;         ((even? exp)
;          (remainder (* (expmod base (/ exp 2) m)
;                        (expmod base (/ exp 2) m))
;                     m))
;         (else
;          (remainder (* base (expmod base
;                                     (- exp 1)
;                                     m))
;                     m))))
;
; "I don't see what difference that could make", says Louis. "I do." says Eva.
; "By writing the procedure like that, you have transformed the Θ(logn) process
; into a Θ(n) process." Explain.

; It is quite similar to the recursive fibonacci. The expmod function halves m
; on every iteration, which would lead to Θ(logn). However, it does perform
; expmod twice, which in turn doubles the work on every step. This leads to
; an Θ(n) order of growth.
