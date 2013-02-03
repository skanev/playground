; EOPL exercise 3.18
;
; Add an expression to the defined language
;
;   Expression ::= unpack {Identifier}* = Expression in Expression
;
; so that unpack x y z = lst in ... binds x, y, and z to the elements of lst
; if lst is a list of exactly three elements, and reports an error otherwise.
; For example, the value of
;
;   let u = 7
;   in unpack x y = cons(u, cons(3, emptylist))
;      in -(x, y)
;
; should be 4.

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
  (cons-exp
    (car expression?)
    (cdr expression?))
  (cond-exp
    (conditions (list-of expression?))
    (actions (list-of expression?)))
  (car-exp
    (expr expression?))
  (cdr-exp
    (expr expression?))
  (null?-exp
    (expr expression?))
  (emptylist-exp)
  (list-exp
    (exprs (list-of expression?)))
  (if-exp
    (predicate expression?)
    (consequent expression?)
    (alternative expression?))
  (var-exp
    (var symbol?))
  (print-exp
    (expr expression?))
  (let-exp
    (vars (list-of symbol?))
    (vals (list-of expression?))
    (body expression?))
  (let*-exp
    (vars (list-of symbol?))
    (vals (list-of expression?))
    (body expression?))
  (unpack-exp
    (names (list-of symbol?))
    (lst expression?)
    (body expression?)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression ("print" expression) print-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; Eval

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (pair-val (car expval?) (cdr expval?))
  (emptylist-val))

(define (expval->num val)
  (cases expval val
    (num-val (num) num)
    (else (eopl:error 'expval->num "Invalid number: ~s" val))))

(define (expval->bool val)
  (cases expval val
    (bool-val (bool) bool)
    (else (eopl:error 'expval->bool "Invalid boolean: ~s" val))))

(define (expval->pair val)
  (cases expval val
    (pair-val (car cdr) (cons car cdr))
    (else (eopl:error 'expval->pair "Invalid pair: ~s" val))))

(define (pair-car val)
  (cases expval val
    (pair-val (car cdr) car)
    (else (eopl:error 'pair-car "Expected a pair: ~s" val))))

(define (pair-cdr val)
  (cases expval val
    (pair-val (car cdr) cdr)
    (else (eopl:error 'pair-cdr "Expected a pair: ~s" val))))

(define (emptylist? val)
  (cases expval val
    (emptylist-val () #t)
    (else #f)))

(define (pair-null? val)
  (cases expval val
    (emptylist-val () (bool-val #t))
    (else (bool-val #f))))

(define (list-val pair)
  (if (null? pair)
      (emptylist-val)
      (pair-val (car pair)
                (list-val (cdr pair)))))

(define (eval-cond conditions actions env)
  (cond ((null? conditions)
         (bool-val #f))
        ((expval->bool (value-of (car conditions) env))
         (value-of (car actions) env))
        (else
         (eval-cond (cdr conditions) (cdr actions) env))))

(define (print-out value)
  (cases expval value
    (num-val (n) (display n))
    (bool-val (b) (display b))
    (emptylist-val () (display "emptylist"))
    (pair-val (head tail)
      (display "cons(")
      (print-out head)
      (display ", ")
      (print-out tail)
      (display ")"))))

(define (eval-let* vars vals body env)
  (if (null? vars)
      (value-of body env)
      (eval-let* (cdr vars)
                 (cdr vals)
                 body
                 (extend-env (car vars)
                             (value-of (car vals) env)
                             env))))

(define (eval-unpack names lst-exp body env)
  (define lst (value-of lst-exp env))
  (define (bind-names vars vals new-env)
    (cond ((and (null? vars) (pair-null? vals))
           new-env)
          ((or (null? vars) (emptylist? vals))
           (eopl:error 'eval-unpack "List size mismatch ~a, ~a" names lst))
          (else
           (bind-names (cdr vars) (pair-cdr vals)
                       (extend-env (car vars) (pair-car vals) new-env)))))
  (value-of body (bind-names names lst env)))


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
    (let-exp (vars vals body)
      (value-of body (extend-env* vars
                                  (map (curryr value-of env) vals)
                                  env)))
    (let*-exp (vars vals body)
      (eval-let* vars vals body env))
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
    (emptylist-exp ()
      (emptylist-val))
    (cons-exp (car cdr)
      (let ((car-val (value-of car env))
            (cdr-val (value-of cdr env)))
        (pair-val car-val cdr-val)))
    (car-exp (expr)
      (pair-car (value-of expr env)))
    (cdr-exp (expr)
      (pair-cdr (value-of expr env)))
    (null?-exp (expr)
      (pair-null? (value-of expr env)))
    (list-exp (exprs)
      (list-val (map (curryr value-of env) exprs)))
    (print-exp (expr)
      (print-out (value-of expr env))
      (num-val 1))
    (unpack-exp (names lst body)
      (eval-unpack names lst body env))
    (cond-exp (conditions actions)
      (eval-cond conditions actions env))))
