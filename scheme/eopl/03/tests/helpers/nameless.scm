(define (run code)
  (eval* code (empty-nameless-env)))

(define (expval->schemeval val)
  (cases expval val
    (num-val (num) num)
    (bool-val (bool) bool)
    (proc-val (proc) proc)
    (else (eopl:error 'eval* "Don't know how to handle expval ~a" val))))

(define (eval* code env)
  (let* ((expr (scan&parse code))
         (expr (translation-of expr (empty-senv)))
         (result (value-of expr env)))
    (expval->schemeval result)))
