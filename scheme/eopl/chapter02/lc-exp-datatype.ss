; Exercise 2.23
(module lc-exp-datatype eopl
  (provide lc-exp? lambda-exp var-exp app-exp)

  (define var-name?
    (lambda (n) (and (symbol? n) (not (equal? 'lambda n)))))

  (define-datatype lc-exp lc-exp?
    (var-exp
      (var var-name?))
    (lambda-exp
      (bound-var var-name?)
      (body lc-exp?))
    (app-exp
      (rator lc-exp?)
      (rand lc-exp?)))
)
