; EOPL exercise 3.11
;
; In a real language, one might have many operators such as those in the
; preceding exercises. Rearrange the code in the interpreter so that it is easy
; to add new operators.

; This is annoyingly tricky because of SLLGEN. I can't figure out why, but I
; can't get it to either dynamically define the grammar with a predefined list
; of operators (when the list is not specified as a literal) and I can't get
; the scanner to treat some words as operators. Instead, I'm going to simplify
; everything. I'm just going to implement the four arithmetic operators. If
; more operators are added, the grammar needs to be modifying accordingly.

(load-relative "cases/let/env.scm")

; The parser

(define-datatype expression expression?
  (const-exp
    (num number?))
  (op-exp
    (operator symbol?)
    (operands (list-of expression?)))
  (if-exp
    (predicate expression?)
    (consequent expression?)
    (alternative expression?))
  (var-exp
    (var symbol?))
  (let-exp
    (var symbol?)
    (value expression?)
    (body expression?)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (operator ((or "+" "-" "*" "/")) symbol)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression (operator "(" (separated-list expression "," ) ")") op-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; Eval

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?)))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (eopl:error 'expval->num "Invalid number: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (eopl:error 'expval->bool "Invalid boolean: ~s" val))))

; The new stuff. Operators are defined in a table and value-of dispatches
; accordingly.

(define operators-table (make-hash))

(define (apply-op rator rands)
  (apply (hash-ref operators-table rator)
         rands))

(define (lift op)
  (lambda args
    (num-val (apply op (map expval->num args)))))

(define operators
  (list (list '+ (lift +))
        (list '- (lift -))
        (list '* (lift *))
        (list '/ (lift quotient))))

(for ([olist operators])
     (hash-set! operators-table (car olist) (cadr olist)))

; The new value-of

(define (value-of expr env)
  (cases expression expr
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (let-exp (var value-exp body)
      (let ((value (value-of value-exp env)))
        (value-of body (extend-env var value env))))
    (if-exp (predicate consequent alternative)
      (let ((value (value-of predicate env)))
        (if (expval->bool value)
            (value-of consequent env)
            (value-of alternative env))))
    (op-exp (rator rands)
      (apply-op rator (map (lambda (e) (value-of e env)) rands)))))
