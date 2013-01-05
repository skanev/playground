; EOPL exercise 2.11
;
; A naive implementation of extend-env* from the preceding exercise requires
; time proportional to k to run. It is possible to represent environments so
; that extend-env* requires only constant time: represent the empty
; environment by the empty list, and represent a non-empty environment by the
; data structure
;
;      +---+---+
;      | o | o ---> saved-env
;      +-|-+---+
;        |
;        V
;      +---+---+
;      | o | o ---> saved-vals
;      +-|-+---+
;        |
;        V
;    saved-vars
;
; Such an environment might look like
;
;                 backbone
;    +---+---+       V        +---+---+             +---+---+
;    | o | o ---------------->| o | o ------------->| o | o ------> rest of environment
;    +-|-+---+                +-|-+---+             +-|-+---+
;      |                        |                     |
;      V                        V                     V
;    +---+---+                +---+---+             +---+---+
;    | o | o ---> (11 12 13)  | o | o ---> (66 77)  | o | o ---> (88 99)
;    +-|-+---+                +-|-+---+             +-|-+---+
;      |                        |                     |
;      V                        V                     V
;   (a b c)                   (x z)                 (x y)
;
; This is called the ribcage representation. The environment is represented as
; a list of pairs called ribs; each left rib is a list of variables and each
; right rib is the corresponding list of values.
;
; Implement the environment interface, including extend-env*, in this
; representation.

(define (empty-env)
  '())

(define (extend-env var val env)
  (extend-env* (list var) (list val) env))

(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

(define (apply-env env var)
  (define (scan vars vals)
    (cond ((null? vars) (apply-env (cdr env) var))
          ((eq? (car vars) var) (car vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (null? env)
      (eopl:error 'apply-env "Variable not found: ~s" var)
      (scan (caar env) (cdar env))))
