; Exercise 2.6 (1/3) -- ([var-list] [value-list])
(module alt-repr-one scheme
  (provide empty-env extend-env apply-env)
  
  (define empty-env (lambda () '(() ())))
  (define apply-env
    (lambda (var env)
      (if (eqv? var (caar env))
          (caadr env)
          (apply-env var (list (cdar env) (cdadr env))))))
  (define extend-env
    (lambda (var val env)
      (list (cons var (car env))
            (cons val (cadr env)))))
)

