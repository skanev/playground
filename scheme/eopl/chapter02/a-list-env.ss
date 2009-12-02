; Exercise 2.5, 2.8, 2.9, 2.10
(module a-list-env eopl
  (provide empty-env apply-env extend-env extend-env* empty-env? has-binding?)

  (define empty-env (lambda () '()))
  (define empty-env? (lambda (env) (null? env)))
  (define apply-env
    (lambda (var env)
      (cond [(null? env) (eopl:error 'apply-env "No binding for ~s" var)]
            [(eqv? (caar env) var) (cadar env)]
            [else (apply-env var (cdr env))]
  )))
  (define extend-env (lambda (var val env) (cons (list var val) env)))
  (define extend-env*
    (lambda (vars vals env)
      (if (null? vars)
          env
          (extend-env (car vars) (car vals) (extend-env* (cdr vars) (cdr vals) env))
  )))
  (define has-binding?
    (lambda (var env)
      (cond [(empty-env? env) #f]
            [(eqv? (caar env) var) #t]
            [else (has-binding? var (cdr env))]
  )))
)
