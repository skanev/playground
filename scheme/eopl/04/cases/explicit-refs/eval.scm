(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (saved-env environment?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env)
      (value-of body (extend-env var val saved-env)))))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?))
  (ref-val
    (ref integer?)))

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

(define (expval->ref val)
  (cases expval val
    (ref-val (num) num)
    (else (eopl:error 'expval->ref "Invalid reference ~s" val))))

(define (value-of expr env)
  (cases expression expr
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (diff-exp (minuend subtrahend)
      (let ((minuend-val (value-of minuend env))
            (subtrahend-val (value-of subtrahend env)))
        (let ((minuend-num (expval->num minuend-val))
              (subtrahend-num (expval->num subtrahend-val)))
          (num-val
            (- minuend-num subtrahend-num)))))
    (zero?-exp (arg)
      (let ((value (value-of arg env)))
        (let ((number (expval->num value)))
          (if (zero? number)
              (bool-val #t)
              (bool-val #f)))))
    (if-exp (predicate consequent alternative)
      (let ((value (value-of predicate env)))
        (if (expval->bool value)
            (value-of consequent env)
            (value-of alternative env))))
    (let-exp (var value-exp body)
      (let ((value (value-of value-exp env)))
        (value-of body
          (extend-env var value env))))
    (proc-exp (var body)
      (proc-val (procedure var body env)))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg)))
    (begin-exp (body)
      (let* ((first (car body))
             (rest (cdr body))
             (value (value-of first env)))
        (if (null? rest)
            value
            (value-of (begin-exp rest) env))))
    (newref-exp (arg)
      (ref-val (newref (value-of arg env))))
    (deref-exp (arg)
      (deref (expval->ref (value-of arg env))))
    (setref-exp (ref-exp value-exp)
      (let ((ref (value-of ref-exp env)))
        (let ((value (value-of value-exp env)))
          (setref! (expval->ref ref) value)
          (num-val 42))))))

; The store

(define the-store 'uninitialized)

(define (empty-store)
  '())

(define (get-store)
  the-store)

(define (initialize-store!)
  (set! the-store (empty-store)))

(define (reference? v)
  (integer? v))

(define (newref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref! ref val)
  (set! the-store
    (let recur ((store1 the-store)
                (ref1 ref))
      (cond ((null? store1)
             (eopl:error 'setref! "Invalid reference ~s in ~s" ref the-store))
            ((zero? ref1)
             (cons val (cdr store1)))
            (else (cons (car store1)
                        (recur (cdr store1) (- ref1 1))))))))
