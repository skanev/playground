; EOPL exercise 3.27
;
; Add a new kind of procedure called a traceproc to the language. A traceproc
; works exactly like a proc, except that it prints a trace message on entry
; and exit.

(load-relative "cases/proc/env.scm")

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
  (traceproc-exp
    (var symbol?)
    (body expression?))
  (call-exp
    (rator expression?)
    (rand expression?)))

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
    (expression ("traceproc" "(" identifier ")" expression) traceproc-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("(" expression expression ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The evaluator

(define-datatype proc proc?
  (procedure
    (var symbol?)
    (body expression?)
    (saved-env environment?)
    (trace? boolean?)))

(define (apply-procedure proc1 val)
  (cases proc proc1
    (procedure (var body saved-env trace?)
      (if trace? (printf "enter: ~a = ~v\n" var val))
      (let ((result (value-of body (extend-env var val saved-env))))
        (if trace? (printf "exit: ~a\n" var))
        result))))

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
      (proc-val (procedure var body env #f)))
    (traceproc-exp (var body)
      (proc-val (procedure var body env #t)))
    (call-exp (rator rand)
      (let ((proc (expval->proc (value-of rator env)))
            (arg (value-of rand env)))
        (apply-procedure proc arg)))))
