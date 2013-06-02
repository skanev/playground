(define environment? (or/c pair? null?))

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (apply-senv senv var)
  (cond ((null? senv) (eopl:error 'apply-senv "Unbound variable: ~a" var))
        ((eqv? var (car senv)) 0)
        (else (+ 1 (apply-senv (cdr senv) var)))))

(define (nameless-environment? x)
  ((list-of expval?) x))

(define (empty-nameless-env)
  '())

(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))

(define (apply-nameless-env nameless-env n)
  (list-ref nameless-env n))

