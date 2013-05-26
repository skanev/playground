; EOPL exercise 3.31
;
; Extend the language above to allow the declaration of a recursive procedure
; of possibly many arguments, as in exercise 3.21.

; The environment

(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var symbol?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-name symbol?)
    (b-vars (list-of symbol?))
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
    (extend-env-rec (p-name b-vars p-body saved-env)
      (if (eqv? search-var p-name)
          (proc-val (procedure b-vars p-body env))
          (apply-env saved-env search-var)))))

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
    (var (list-of symbol?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rand (list-of expression?)))
  (letrec-exp
    (p-name symbol?)
    (b-vars (list-of symbol?))
    (p-body expression?)
    (letrec-body expression?)))

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
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("letrec" identifier "(" (separated-list identifier ",") ")" "=" expression "in" expression) letrec-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The evaluator

(define-datatype proc proc?
  (procedure
    (vars (list-of symbol?))
    (body expression?)
    (saved-env environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (vars body saved-env)
      (value-of body (extend-env* vars vals saved-env)))))

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
    (proc-exp (vars body)
      (proc-val (procedure vars body env)))
    (call-exp (rator rands)
      (let ((proc (expval->proc (value-of rator env)))
            (args (map (lambda (rand) (value-of rand env))
                       rands)))
        (apply-procedure proc args)))
    (letrec-exp (p-name b-vars p-body letrec-body)
      (value-of letrec-body
        (extend-env-rec p-name b-vars p-body env)))))

