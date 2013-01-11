(define (run code)
  (eval* code (empty-env)))

(define (eval* code env)
  (let* ((expr (scan&parse code))
         (result (value-of expr env)))
    (cases expval result
      (num-val (num) num)
      (bool-val (bool) bool))))

(define (env vars vals)
  (extend-env* vars
               (map (lambda (val)
                      (if (boolean? val)
                          (bool-val val)
                          (num-val val)))
                    vals)
               (empty-env)))
