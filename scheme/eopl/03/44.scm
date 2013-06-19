; EOPL exercise 3.44
;
; In the preceding example, the only use of f is as a known procedure.
; Therefore the procedure built by the expression proc (y) -(y, x) is never
; used. Modify the translator so that such a procedure is never constructred.

; We base this on the previous exercise. We just need to modify how let treats
; its value-exp. If value-exp is a procedure and the var is used only in
; operator position within the let body, then instead of translating the right
; side of the let, we put a sentinel value (unused-proc-exp) in there.

; We can, of course, interpret the exercise as having to inline the procedure,
; but I don't want to go as far.

; The known environment

(define (empty-kenv)
  '())

(define (apply-kenv kenv var)
  (let ((pair (assoc var kenv)))
    (if pair
        (cadr pair)
        (eopl:error 'apply-kenv "Unknown variable: ~a" var))))

(define (kenv-defines? kenv var)
  (if (assoc var kenv)
      #t
      #f))

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
  (known-proc-exp
    (procedure expval?))
  (known-proc-call-exp
    (procedure expval?)
    (argument expression?))
  (unused-proc-exp))

(define-datatype expval expval?
  (num-val
    (num number?))
  (bool-val
    (bool boolean?))
  (proc-val
    (proc proc?)))

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

; The new code

(define (var-exp? expr)
  (cases expression expr
    (var-exp (name) #t)
    (else #f)))

(define (proc-val? val)
  (cases expval val
    (proc-val (proc) #t)
    (else #f)))

(define (procedure-safe-to-remove? body var kenv)
  (and (kenv-defines? kenv var)
       (proc-val? (apply-kenv kenv var))
       (used-only-as-operator? body var)))

(define (used-only-as-operator? expr var)
  (cases expression expr
    (const-exp (num) #t)
    (var-exp (name)
      (not (eqv? var name)))
    (diff-exp (minuend subtrahend)
      (and (used-only-as-operator? minuend var)
           (used-only-as-operator? subtrahend var)))
    (zero?-exp (arg)
      (used-only-as-operator? arg var))
    (if-exp (predicate consequent alternative)
      (and (used-only-as-operator? predicate var)
           (used-only-as-operator? consequent var)
           (used-only-as-operator? alternative var)))
    (let-exp (let-name value-exp body)
      (and (used-only-as-operator? value-exp var)
           (or (eqv? let-name var)
               (used-only-as-operator? value-exp var))))
    (proc-exp (param body)
      (or (eqv? param var)
          (used-only-as-operator? body var)))
    (call-exp (rator rand)
      (and (or (var-exp? rator)
               (used-only-as-operator? rator var))
           (used-only-as-operator? rand var)))
    (else (eopl:error 'used-only-as-operator? "Unexpected expression: ~a" expr))))

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
      (if (constant? value-exp kenv)
          (let* ((value (eval-const value-exp senv kenv))
                 (body-kenv (extend-kenv kenv var value))
                 (body-senv (extend-senv var senv)))
            (nameless-let-exp
              (if (procedure-safe-to-remove? body var body-kenv)
                  (unused-proc-exp)
                  (known-proc-exp value))
              (translation-of body body-senv body-kenv)))
          (nameless-let-exp
            (translation-of value-exp senv kenv)
            (translation-of body (extend-senv var senv) kenv))))
    (proc-exp (var body)
      (nameless-proc-exp
        (translation-of body (extend-senv var senv) kenv)))
    (call-exp (rator rand)
      (let ((proc (known-procedure-ref kenv rator)))
        (if proc
            (known-proc-call-exp
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
    (known-proc-exp (proc)
      proc)
    (known-proc-call-exp (proc rand)
      (apply-procedure (expval->proc proc)
                       (value-of rand nenv)))
    (unused-proc-exp () 'unused-procedure)
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
