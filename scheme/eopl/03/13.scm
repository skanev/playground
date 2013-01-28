; EOPL exercise 3.13
;
; Change the values of the language so that integers are the only expressed
; values. Modify if so that the value 0 is treated as false and all other
; values are treated as true. Modify the predicates accordingly.

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
  (cond-exp
    (conditions (list-of expression?))
    (actions (list-of expression?)))
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
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
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

(define (eval-cond conditions actions env)
  (cond ((null? conditions)
         0)
        ((zero? (value-of (car conditions) env))
         (eval-cond (cdr conditions) (cdr actions) env))
        (else
         (value-of (car actions) env))))

(define (value-of expr env)
  (cases expression expr
    (const-exp (num) num)
    (var-exp (var) (apply-env env var))
    (add-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (+ left-val right-val)))
    (diff-exp (minuend subtrahend)
      (let ((minuend-val (value-of minuend env))
            (subtrahend-val (value-of subtrahend env)))
        (- minuend-val subtrahend-val)))
    (mult-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (* left-val right-val)))
    (div-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (quotient left-val right-val)))
    (minus-exp (arg)
      (- (value-of arg env)))
    (zero?-exp (arg)
      (let ((value (value-of arg env)))
        (if (zero? value)
            1
            0)))
    (if-exp (predicate consequent alternative)
      (let ((value (value-of predicate env)))
        (if (not (zero? value))
            (value-of consequent env)
            (value-of alternative env))))
    (let-exp (var value-exp body)
      (let ((value (value-of value-exp env)))
        (value-of body
          (extend-env var value env))))
    (equal?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (if (= left-val right-val)
            1
            0)))
    (less?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (if (< left-val right-val)
            1
            0)))
    (greater?-exp (left right)
      (let ((left-val (value-of left env))
            (right-val (value-of right env)))
        (if (> left-val right-val)
            1
            0)))
    (cond-exp (conditions actions)
      (eval-cond conditions actions env))))
