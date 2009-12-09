; Exercise 2.11
(module ribcage eopl
  (provide empty-env apply-env extend-env extend-env*)

  (define empty-env (lambda () '()))
  (define extend-env* (lambda (vars vals env) (cons (list vars vals) env)))
  (define extend-env (lambda (var val env) (extend-env* (list var) (list val) env)))
  (define apply-env
    (lambda (var env)
      (let in-rib ([rib (car env)])
        (cond [(null? (car rib)) (apply-env var (cdr env))]
              [(equal? (caar rib) var) (caadr rib)]
              [else (in-rib (list (cdar rib) (cdadr rib)))]))))
)
