; EOPL exercise 3.08
;
; Add a numeric equality predicate equal? and numeric order predicates
; greater? and less? to the set of operations in the defined language.

(load-relative "cases/let/env.scm")

; The parser

(define-datatype expression expression?
  (const-exp
    (num number?))
  (minus-exp
    (expr expression?))
  (add-exp
    (left expression?)
    (right expression?))
  (diff-exp
    (left expression?)
    (right expression?))
  (mult-exp
    (left expression?)
    (right expression?))
  (div-exp
    (left expression?)
    (right expression?))
  (zero?-exp
    (expr expression?))
  (equal?-exp
    (left expression?)
    (right expression?))
  (less?-exp
    (left expression?)
    (right expression?))
  (greater?-exp
    (left expression?)
    (right expression?))
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
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
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

(define (value-of expr env)
  (cases expression expr
    (const-exp (num) (num-val num))
    (var-exp (var) (apply-env env var))
    (add-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (let ((left-num (expval->num left-val))
              (right-num (expval->num right-val)))
          (num-val (+ left-num right-num)))))
    (diff-exp (minuend subtrahend)
      (let ((minuend-val (value-of minuend env))
            (subtrahend-val (value-of subtrahend env)))
        (let ((minuend-num (expval->num minuend-val))
              (subtrahend-num (expval->num subtrahend-val)))
          (num-val (- minuend-num subtrahend-num)))))
    (mult-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (let ((left-num (expval->num left-val))
              (right-num (expval->num right-val)))
          (num-val (* left-num right-num)))))
    (div-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (let ((left-num (expval->num left-val))
              (right-num (expval->num right-val)))
          (num-val (quotient left-num right-num)))))
    (minus-exp (arg)
      (num-val (- (expval->num (value-of arg env)))))
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
    (equal?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (bool-val (= (expval->num left-val)
                     (expval->num right-val)))))
    (less?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (bool-val (< (expval->num left-val)
                     (expval->num right-val)))))
    (greater?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (bool-val (> (expval->num left-val)
                     (expval->num right-val)))))
    ))
