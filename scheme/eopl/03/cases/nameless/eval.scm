(define-datatype proc proc?
  (procedure
    (body expression?)
    (saved-nameless-env nameless-environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (body saved-nameless-env)
      (value-of body (extend-nameless-env val saved-nameless-env)))))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (eopl:error 'expval->num "Invalid number: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (eopl:error 'expval->bool "Invalid boolean: ~s" val))))

(define (expval->proc val)
  (cases expval val
    (proc-val (proc) proc)
    (else (eopl:error 'expval->proc "Invalid procedure: ~s" val))))

(define (translation-of expr senv)
  (cases expression expr
    (const-exp (num) (const-exp num))
    (diff-exp (minuend subtrahend)
      (diff-exp
        (translation-of minuend senv)
        (translation-of subtrahend senv)))
    (zero?-exp (arg)
      (zero?-exp (translation-of arg senv)))
    (if-exp (predicate consequent alternative)
      (if-exp
        (translation-of predicate senv)
        (translation-of consequent senv)
        (translation-of alternative senv)))
    (var-exp (var)
      (nameless-var-exp (apply-senv senv var)))
    (let-exp (var value-exp body)
      (nameless-let-exp
        (translation-of value-exp senv)
        (translation-of body (extend-senv var senv))))
    (proc-exp (var body)
      (nameless-proc-exp
        (translation-of body (extend-senv var senv))))
    (call-exp (rator rand)
      (call-exp
        (translation-of rator senv)
        (translation-of rand senv)))
    (else
      (eopl:error 'translation-of "Cannot translate ~a" expr))))

(define (value-of expr nenv)
  (cases expression expr
    (const-exp (num) (num-val num))
    (diff-exp (minuend subtrahend)
      (let ((minuend-val (value-of minuend nenv))
            (subtrahend-val (value-of subtrahend nenv)))
        (let ((minuend-num (expval->num minuend-val))
              (subtrahend-num (expval->num subtrahend-val)))
          (num-val
            (- minuend-num subtrahend-num)))))
    (zero?-exp (arg)
      (let ((value (value-of arg nenv)))
        (let ((number (expval->num value)))
          (if (zero? number)
              (bool-val #t)
              (bool-val #f)))))
    (if-exp (predicate consequent alternative)
      (let ((value (value-of predicate nenv)))
        (if (expval->bool value)
            (value-of consequent nenv)
            (value-of alternative nenv))))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator nenv)))
            (arg (value-of rand nenv)))
        (apply-procedure proc arg)))
    (nameless-var-exp (n)
      (apply-nameless-env nenv n))
    (nameless-let-exp (value-exp body)
      (let ((val (value-of value-exp nenv)))
        (value-of body
                  (extend-nameless-env val nenv))))
    (nameless-proc-exp (body)
      (proc-val
        (procedure body nenv)))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))
