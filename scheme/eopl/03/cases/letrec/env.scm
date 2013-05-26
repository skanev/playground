(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-name symbol?)
    (b-var symbol?)
    (body expression?)
    (env environment?)))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(define (apply-env env search-var)
  (cases environment env
    (empty-env ()
      (eopl:error 'apply-env "Variable not found: ~s" search-var))
    (extend-env (saved-var saved-val saved-env)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))
    (extend-env-rec (p-name b-var p-body saved-env)
      (if (eqv? search-var p-name)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-var)))))
