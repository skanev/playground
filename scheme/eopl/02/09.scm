; EOPL exercise 2.09
;
; Add to the environment interface an observer called has-binding? that takes
; an environment env and a variable s and tests to see if s has an associated
; value in env. Implement using the a-list representation.

(load-relative "05.scm")

(define (has-binding? env var)
  (cond ((null? env) #f)
        ((eqv? (caar env) var) #t)
        (else (has-binding? (cdr env) var))))
