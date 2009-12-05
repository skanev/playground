; Exercise 2.6 (3/3) -- ((var.val) (var2.val2) ...)
(module alt-repr-three scheme
  (provide empty-env extend-env apply-env)
  
  (define empty-env (lambda () '()))
  (define apply-env
    (lambda (var env)
      (if (eqv? var (caar env))
          (cdar env)
          (apply-env var (cdr env)))))
  (define extend-env
    (lambda (var val env)
      (cons (cons var val) env)))
)

