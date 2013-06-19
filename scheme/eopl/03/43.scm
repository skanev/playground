; EOPL exercise 3.43
;
; The translator can do more than keep track of the names of variables. For
; example, consider the program:
;
;   let x = 3
;   in let f = proc (y) -(y, x)
;      in (f 13)
;
; Here we can tell statically that at the procedure call, f will be bound to a
; procedure whose body is -(y, x), where x has the same value that it had at
; the procedure-creation site. Therefore we could avoid looking up f in the
; environment entirely. Extend the translator to keep track of "known
; procedures" and generate code that avoids an environment lookup at the call
; of such a procedure.

; Let's start with a "known environment". It will store all values that can be
; statically determined. Whenever we have a call with an operator that can be
; determined via the known environment, we will translate it to a
; known-proc-call (which is a new variant in the expression datatype).

(define (empty-kenv)
  '())

(define (apply-kenv kenv var)
  (let ((pair (assoc var kenv)))
    (if pair
        (cadr pair)
        (eopl:error 'apply-kenv "Unknown variable: ~a" var))))

(define (extend-kenv kenv var value)
  (cons (list var value)
        kenv))

(define (kenv-names kenv)
  (map car kenv))

; The data type needs to be pulled up, because of, marcos

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
    (body expression?))
  (known-proc-call
    (procedure expval?)
    (argument expression?)))

; Some functions to work with known environments

(define (construct-nenv senv kenv)
  (map (curry apply-kenv kenv) senv))

(define (constant? expr kenv)
  (null? (free-variables expr (kenv-names kenv))))

(define (eval-const expr senv kenv)
  (value-of (translation-of expr senv (empty-kenv))
            (construct-nenv senv kenv)))

(define (known-procedure-ref kenv expr)
  (cases expression expr
    (var-exp (name)
      (let ((pair (assoc name kenv)))
        (if pair
            (cadr pair)
            #f)))
    (else #f)))

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

(define (translation-of expr senv kenv)
  (cases expression expr
    (const-exp (num) (const-exp num))
    (diff-exp (minuend subtrahend)
      (diff-exp
        (translation-of minuend senv kenv)
        (translation-of subtrahend senv kenv)))
    (zero?-exp (arg)
      (zero?-exp (translation-of arg senv kenv)))
    (if-exp (predicate consequent alternative)
      (if-exp
        (translation-of predicate senv kenv)
        (translation-of consequent senv kenv)
        (translation-of alternative senv kenv)))
    (var-exp (var)
      (nameless-var-exp (apply-senv senv var)))
    (let-exp (var value-exp body)
      (nameless-let-exp
        (translation-of value-exp senv kenv)
        (translation-of body
                        (extend-senv var senv)
                        (if (constant? value-exp kenv)
                            (extend-kenv kenv var (eval-const value-exp senv kenv))
                            kenv))))
    (proc-exp (var body)
      (nameless-proc-exp
        (translation-of body (extend-senv var senv) kenv)))
    (call-exp (rator rand)
      (let ((proc (known-procedure-ref kenv rator)))
        (if proc
            (known-proc-call
              proc
              (translation-of rand senv kenv))
            (call-exp
              (translation-of rator senv kenv)
              (translation-of rand senv kenv)))))
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
    (known-proc-call (proc rand)
      (apply-procedure (expval->proc proc)
                       (value-of rand nenv)))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))

; Free-variables

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
