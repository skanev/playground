; EOPL exercise 2.08
;
; Add to the environment interface an observer called empty-env? and implement
; it using the a-list representation.

(load-relative "05.scm")

(define (empty-env? env)
  (null? env))
