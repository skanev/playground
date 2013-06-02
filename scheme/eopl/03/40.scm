; EOPL exercise 3.40
;
; Extend the lexical address translator and interpreter to handle letrec. Do
; this by modifying the context argument of to translation-of so that it keeps
; track of not only the name of each bound variable, but also whether it was
; bound by letrec or not. For a reference to a variable that was bound by a
; letrec, generate a new kind of reference, called a nameless-letrec-var-exp.
; You can then continue to use the nameless environment representation above,
; and the interpreter can do the right thing with a nameless-letrec-var-exp.

; This is slightly awkward. I'm certain that I would have produced better
; results if I could tweak the nameless environment a little bit.
;
; The most annoying thing is that I need to box the procedure body into a
; proc, nested in a proc-val, so I can maintain the constraint on
; nameless-environment.

; The environment

(define environment? (or/c pair? null?))

(define (empty-senv)
  '())

(define (extend-senv type var senv)
  (cons (list type var) senv))

(define (extend-senv-lambda var senv)
  (extend-senv 'lambda var senv))

(define (extend-senv-letrec var senv)
  (extend-senv 'letrec var senv))

(define (apply-senv senv var)
  (let recur ((index 0)
              (senv senv))
    (cond ((null? senv) (eopl:error 'apply-senv "Unbound variable: ~a" var))
          ((eqv? var (cadar senv)) (values (caar senv) index))
          (else (recur (+ index 1) (cdr senv))))))

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
  (letrec-exp
    (p-name symbol?)
    (b-var symbol?)
    (p-body expression?)
    (letrec-body expression?))
  (proc-exp
    (var symbol?)
    (body expression?))
  (call-exp
    (rator expression?)
    (rand expression?))
  (nameless-var-exp
    (num integer?))
  (nameless-letrec-var-exp
    (num integer?))
  (nameless-let-exp
    (exp1 expression?)
    (body expression?))
  (nameless-letrec-exp
    (p-body expression?)
    (letrec-body expression?))
  (nameless-proc-exp
    (body expression?)))

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
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
    (expression ("(" expression expression ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The evaluator

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
      (let-values (((type index) (apply-senv senv var)))
        (case type
          ((lambda) (nameless-var-exp index))
          ((letrec) (nameless-letrec-var-exp index))
          (else (eopl:error 'value-of "Unknown variable type: ~a" type)))))
    (let-exp (var value-exp body)
      (nameless-let-exp
        (translation-of value-exp senv)
        (translation-of body (extend-senv-lambda var senv))))
    (letrec-exp (p-name b-var p-body letrec-body)
      (nameless-letrec-exp
        (translation-of p-body (extend-senv-lambda b-var (extend-senv-letrec p-name senv)))
        (translation-of letrec-body (extend-senv-letrec p-name senv))))
    (proc-exp (var body)
      (nameless-proc-exp
        (translation-of body (extend-senv-lambda var senv))))
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
    (nameless-letrec-var-exp (n)
      (let* ((new-nenv (drop nenv n))
             (proc-obj (expval->proc (car new-nenv))))
        (cases proc proc-obj
          (procedure (body saved-env)
            (proc-val (procedure body new-nenv)))
          (else (eopl:error 'value-of "Expected a procedure")))))
    (nameless-let-exp (value-exp body)
      (let ((val (value-of value-exp nenv)))
        (value-of body
                  (extend-nameless-env val nenv))))
    (nameless-letrec-exp (proc-body letrec-body)
      (let ((the-proc (proc-val (procedure proc-body nenv))))
        (value-of letrec-body
                  (extend-nameless-env the-proc nenv))))
    (nameless-proc-exp (body)
      (proc-val
        (procedure body nenv)))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))
