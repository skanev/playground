; Exercise 2.21
(module env-with-datatype eopl
  (provide empty-env extend-env apply-env
           has-binding?)

  (define-datatype binding binding?
    (empty-env)
    (extend-env
      (var symbol?)
      (value integer?)
      (env binding?)))

  (define apply-env
    (lambda (env search-var)
      (cases binding env
        (empty-env () (eopl:error "Variable ~s not found in environment" search-var))
        (extend-env (var value nested-env)
          (if (equal? var search-var)
            value
            (apply-env nested-env search-var))))))

  (define has-binding?
    (lambda (env search-var)
      (cases binding env
        (empty-env () #f)
        (extend-env (var value nested-env)
          (if (equal? var search-var)
            #t
            (has-binding? nested-env search-var))))))

)
