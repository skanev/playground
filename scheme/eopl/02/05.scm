; EOPL exercise 2.05
;
; We can use any data structure for representing environments, if we can
; distinguish empty environments from non-empty ones, and in which one can
; extract the pieces of a non-empty environment. Implement environments using
; a representation in which the empty environment is represented as the empty
; list, and in which extend-env builds an environment that looks like
;
;     +---+---+
;     | o | o ---> saved-env
;     +-|-+---+
;       |
;       V
;     +---+---+
;     | o | o ---> saved-val
;     +-|-+---+
;       |
;       V
;   saved-var
;
; This is called an a-list or association-list representation.

(define (empty-env)
  '())

(define (extend-env var val env)
  (cons (cons var val)
        env))

(define (apply-env env var)
  (cond ((null? env) (eopl:error 'apply-env "Variable not found"))
        ((eqv? (caar env) var) (cdar env))
        (else (apply-env (cdr env) var))))
