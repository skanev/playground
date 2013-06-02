(define (run code)
  (eval* code (empty-nameless-env)))

(define (eval* code env)
  (let* ((expr (scan&parse code))
         (expr (translation-of expr (empty-senv)))
         (result (value-of expr env)))
    (cases expval result
      (num-val (num) num)
      (bool-val (bool) bool)
      (proc-val (proc) proc))))
