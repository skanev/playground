; EOPL exercise 3.41
;
; Modify the lexical address translator and interpreter to handle let
; expressions, procedures and procedure calls with multiple arguments, as in
; exercise 3.21. Do this using a nameless version of the ribcage
; representation of environments (exercise 2.11). For this representation, the
; lexical address will consist of two nonnegative integers: the lexical depth,
; to indicate the number of contours crossed, as before; and a position, to
; indicate the position of the variable in the declaration.

; The environments

(define environment? (or/c pair? null?))

(define (empty-senv)
  '())

(define (extend-senv vars senv)
  (cons vars senv))

(define (apply-senv senv var)
  (let recur ((depth 0)
              (position 0)
              (frames (cdr senv))
              (vars (car senv)))
    (cond ((and (null? vars) (null? frames)) (eopl:error 'apply-senv "Unbound variable: ~a" var))
          ((null? vars) (recur (+ depth 1) 0 (cdr frames) (car frames)))
          ((eq? var (car vars)) (list depth position))
          (else (recur depth (+ position 1) frames (cdr vars))))))

(define (nameless-environment? x)
  ((list-of (list-of expval?)) x))

(define (empty-nameless-env)
  '())

(define (extend-nameless-env vals nameless-env)
  (cons vals nameless-env))

(define (apply-nameless-env nameless-env depth position)
  (list-ref (list-ref nameless-env depth) position))

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
    (vars (list-of symbol?))
    (vals (list-of expression?))
    (body expression?))
  (proc-exp
    (vars (list-of symbol?))
    (body expression?))
  (call-exp
    (rator expression?)
    (rands (list-of expression?)))
  (nameless-var-exp
    (depth integer?)
    (position integer?))
  (nameless-let-exp
    (exps (list-of expression?))
    (body expression?))
  (nameless-proc-exp
    (arity integer?)
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
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; The evaluator

(define-datatype proc proc?
  (procedure
    (arity integer?)
    (body expression?)
    (saved-nameless-env nameless-environment?)))

(define (apply-procedure proc1 vals)
  (cases proc proc1
    (procedure (arity body saved-nameless-env)
      (if (= (length vals) arity)
          (value-of body (extend-nameless-env vals saved-nameless-env))
          (eopl:error 'apply-procedure "Wrong arity")))))

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
      (apply nameless-var-exp (apply-senv senv var)))
    (let-exp (vars vals-exps body)
      (nameless-let-exp
        (map (curryr translation-of senv) vals-exps)
        (translation-of body (extend-senv vars senv))))
    (proc-exp (vars body)
      (nameless-proc-exp
        (length vars)
        (translation-of body (extend-senv vars senv))))
    (call-exp (rator rands)
      (call-exp
        (translation-of rator senv)
        (map (curryr translation-of senv) rands)))
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
    (call-exp (rator rands)
      (let ((proc (expval->proc (value-of rator nenv)))
            (args (map (curryr value-of nenv) rands)))
        (apply-procedure proc args)))
    (nameless-var-exp (depth position)
      (apply-nameless-env nenv depth position))
    (nameless-let-exp (val-exps body)
      (let ((vals (map (curryr value-of nenv) val-exps)))
        (value-of body
                  (extend-nameless-env vals nenv))))
    (nameless-proc-exp (arity body)
      (proc-val
        (procedure arity body nenv)))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))
