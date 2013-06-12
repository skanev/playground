; EOPL exercise 3.42
;
; Modify the lexical address translator and intrepreter to used the trimmed
; representation of procedures from exercise 3.26. For this, you will need to
; translate the body of the procedure not (extend-senv var senv), but in a new
; static environment that tells exactly where each variable will be kept in
; the trimmed representation.

; The environment

(define environment? (or/c pair? null?))

(define (empty-senv)
  '())

(define (extend-senv var senv)
  (cons var senv))

(define (apply-senv senv var)
  (cond ((null? senv) (eopl:error 'apply-senv "Unbound variable: ~a" var))
        ((eqv? var (car senv)) 0)
        (else (+ 1 (apply-senv (cdr senv) var)))))

(define (nameless-environment? x)
  ((list-of expval?) x))

(define (empty-nameless-env)
  '())

(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))

(define (apply-nameless-env nameless-env n)
  (list-ref nameless-env n))

; The parser

(define-datatype expression expression?
  (const-exp
    (num number?))
  (diff-exp
    (minuend expression?)
    (subtrahend expression?))
  (zero?-exp
    (expr expression?))
  (if-exp
    (predicate expression?)
    (consequent expression?)
    (alternative expression?))
  (var-exp
    (var symbol?))
  (let-exp
    (var symbol?)
    (value expression?)
    (body expression?))
  (proc-exp
    (var symbol?)
    (body expression?))
  (call-exp
    (rator expression?)
    (rand expression?))
  (nameless-var-exp
    (num integer?))
  (nameless-let-exp
    (exp1 expression?)
    (body expression?))
  (nameless-proc-exp
    (body expression?)
    (indices (list-of integer?))))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("(" expression expression ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The interpreter

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
      (let* ((vars (free-variables body (list var)))
             (indices (map (curry position senv) vars)))
        (nameless-proc-exp
          (translation-of body (extend-senv var vars))
          indices)))
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
    (nameless-proc-exp (body indices)
      (proc-val
        (procedure body (slice-nenv nenv indices))))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))

; The new code

(define (slice-nenv nenv indices)
  (map (curry apply-nameless-env nenv) indices))

(define (position lst elem)
  (cond ((null? lst) (eopl:error 'position "Empty list"))
        ((eqv? (car lst) elem) 0)
        (else (+ 1 (position (cdr lst) elem)))))

(define (free-variables expr bound)
  (remove-duplicates
    (let recur ((expr expr) (bound bound))
      (cases expression expr
             (const-exp (num) '())
             (var-exp (var)
               (if (memq var bound)
                   '()
                   (list var)))
             (diff-exp (minuend subtrahend)
               (append (recur minuend bound)
                       (recur subtrahend bound)))
             (zero?-exp (arg)
               (recur arg bound))
             (if-exp (predicate consequent alternative)
               (append (recur predicate bound)
                       (recur consequent bound)
                       (recur alternative bound)))
             (let-exp (var value-exp body)
               (append (recur value-exp bound)
                       (recur body (cons var bound))))
             (proc-exp (var body)
               (recur body (cons var bound)))
             (call-exp (rator rand)
                (append (recur rator bound)
                        (recur rand bound)))
             (else
               (eopl:error 'free-variables "Can't find variables in: ~a" expr))))))
