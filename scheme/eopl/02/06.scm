; EOPL exercise 2.06
;
; Invent at least three different representations of the environment interface
; and implement them.

; 1. 2-list representation
;    ((a 1) (b 2) (c 3) ...)

(define empty-env '())
(define extend-env '())
(define apply-env '())

(define (use-2-list)
  (set! empty-env
    (lambda () '()))

  (set! extend-env
    (lambda (var val env)
      (cons (list var val)
            env)))

  (set! apply-env
    (lambda (env var)
      (cond ((null? env) (eopl:error 'apply-env "Variable not found"))
            ((eqv? (caar env) var) (cadar env))
            (else (apply-env (cdr env) var))))))

; 2. two lists representation
;    ((a b c ...) (1 2 3 ...))

(define (use-two-lists)
  (set! empty-env
    (lambda () '(() ())))

  (set! extend-env
    (lambda (var val env)
      (list (cons var (car env))
            (cons val (cadr env)))))

  (set! apply-env
    (lambda (env var)
      (cond ((null? (car env)) (eopl:error 'apply-env "Variable not found"))
            ((eqv? (caar env) var) (caadr env))
            (else (apply-env (list (cdar env) (cdadr env)) var))))))

; 2. var-val list
;    (a 1 b 2 c 3 ...)

(define (use-var-val-list)
  (set! empty-env
    (lambda () '()))

  (set! extend-env
    (lambda (var val env)
      (cons var
            (cons val env))))

  (set! apply-env
    (lambda (env var)
      (cond ((null? env) (eopl:error 'apply-env "Variable not found"))
            ((eqv? (car env) var) (cadr env))
            (else (apply-env (cddr env) var))))))
