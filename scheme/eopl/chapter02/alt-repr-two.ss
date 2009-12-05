; Exercise 2.6 (2/3) -- (var1 val1 var2 val2 ...)
(module alt-repr-two scheme
  (provide empty-env extend-env apply-env)
  
  (define empty-env (lambda () '()))
  (define apply-env
    (lambda (var env)
      (if (eqv? var (car env))
          (cadr env)
          (apply-env var (cddr env)))))
  (define extend-env
    (lambda (var val env)
      (cons var (cons val env))))
)

