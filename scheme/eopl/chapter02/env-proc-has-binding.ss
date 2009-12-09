; Exercise 2.14
(module env-proc-has-binding eopl
  (provide empty-env apply-env extend-env empty-env? has-binding?)

  (define empty-env
    (lambda ()
      (list
        (lambda (var) (eopl:error "Variable ~s not found in environment" var))
        (lambda () #t)
        (lambda (var) #f))))
  (define empty-env? (lambda (env) ((cadr env))))
  (define extend-env
    (lambda (var val env)
      (list
        (lambda (searched-var)
          (if (equal? var searched-var)
              val
              (apply-env searched-var env)))
        (lambda () #f)
        (lambda (searched-var)
          (or (equal? var searched-var)
              ((caddr env) searched-var))))))
  (define has-binding?
    (lambda (var env)
      ((caddr env) var)))
  (define apply-env
    (lambda (var env)
      ((car env) var)))
)
