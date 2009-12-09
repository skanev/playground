; Exercise 2.13
(module env-proc eopl
  (provide empty-env apply-env extend-env empty-env?)

  (define empty-env
    (lambda ()
      (list
        (lambda (var) (eopl:error "Variable ~s not found in environment" var))
        (lambda () #t))))
  (define empty-env? (lambda (env) ((cadr env))))
  (define extend-env
    (lambda (var val env)
      (list
        (lambda (searched-var)
          (if (equal? var searched-var)
              val
              (apply-env searched-var env)))
        (lambda () #f))))
  (define apply-env
    (lambda (var env)
      ((car env) var)))
)
