; EOPL exercise 3.34
;
; Implement extend-env-rec in the procedural representation of environment
; from section 2.2.3.

(load-relative "cases/letrec/parser.scm")
(load-relative "cases/letrec/eval.scm")

(define (environment? env)
  (procedure? env))

(define (empty-env)
  (lambda (search-var)
    (eopl:error 'apply-env "Variable not found: ~s" var)))

(define (extend-env var val env)
  (lambda (search-var)
    (if (eqv? search-var var)
        val
        (env search-var))))

(define (extend-env-rec name var body env)
  (lambda (search-var)
    (if (eqv? search-var name)
        (proc-val (procedure var body (extend-env-rec name var body env)))
        (env search-var))))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(define (apply-env env search-var)
  (env search-var))
