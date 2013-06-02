; EOPL exercise 3.39
;
; Extend the lexical address translator and interpreter to handle pack and
; unpack from exercise 3.18.

; The environments

(load-relative "cases/nameless/env.scm")

(define (extend-senv* vars senv)
  (append vars senv))

(define (extend-nenv* vals nenv)
  (append vals nenv))

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
  (cond-exp
    (conditions (list-of expression?))
    (actions (list-of expression?)))
  (cons-exp
    (car expression?)
    (cdr expression?))
  (car-exp
    (expr expression?))
  (cdr-exp
    (expr expression?))
  (null?-exp
    (expr expression?))
  (emptylist-exp)
  (list-exp
    (exprs (list-of expression?)))
  (unpack-exp
    (names (list-of symbol?))
    (lst expression?)
    (body expression?))
  (nameless-unpack-exp
    (size integer?)
    (lst expression?)
    (body expression?)))

(define scanner-spec
  '((white-sp (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

(define grammar
  '((expression (number) const-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
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
  (num-val (num number?))
  (bool-val (bool boolean?))
  (pair-val (car expval?) (cdr expval?))
  (emptylist-val)
  (proc-val (proc proc?)))

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

(define (expval->pair val)
  (cases expval val
    (emptylist-val () '())
    (pair-val (car cdr) (cons car (expval->pair cdr)))
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

(define (list-val pair)
  (if (null? pair)
      (emptylist-val)
      (pair-val (car pair)
                (list-val (cdr pair)))))

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
      (nameless-var-exp (apply-senv senv var)))
    (let-exp (var value-exp body)
      (nameless-let-exp
        (translation-of value-exp senv)
        (translation-of body (extend-senv var senv))))
    (proc-exp (var body)
      (nameless-proc-exp
        (translation-of body (extend-senv var senv))))
    (call-exp (rator rand)
      (call-exp
        (translation-of rator senv)
        (translation-of rand senv)))
    (cond-exp (conditions actions)
      (cond-exp
        (map (lambda (e) (translation-of e senv)) conditions)
        (map (lambda (e) (translation-of e senv)) actions)))
    (emptylist-exp ()
      (emptylist-exp))
    (null?-exp (arg)
      (null?-exp (translation-of arg senv)))
    (cons-exp (car cdr)
      (cons-exp
        (translation-of car senv)
        (translation-of cdr senv)))
    (car-exp (lst)
      (car-exp (translation-of lst senv)))
    (cdr-exp (lst)
      (cdr-exp (translation-of lst senv)))
    (list-exp (exprs)
      (list-exp (map (lambda (e) (translation-of e senv)) exprs)))
    (unpack-exp (vars lst body)
      (nameless-unpack-exp
        (length vars)
        (translation-of lst senv)
        (translation-of body (extend-senv* vars senv))))
    (else
      (eopl:error 'translation-of "Cannot translate ~a" expr))))

(define (eval-cond conditions actions nenv)
  (cond ((null? conditions)
         (bool-val #f))
        ((expval->bool (value-of (car conditions) nenv))
         (value-of (car actions) nenv))
        (else
         (eval-cond (cdr conditions) (cdr actions) nenv))))

(define (eval-unpack size list-exp body nenv)
  (let ((vals (expval->pair (value-of list-exp nenv))))
    (if (eqv? (length vals) size)
        (value-of body (extend-nenv* vals nenv))
        (eopl:error 'eval-unpack "Size mismatch: ~a ~a" size vals))))

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
    (cond-exp (conditions actions)
      (eval-cond conditions actions nenv))
    (emptylist-exp ()
      (emptylist-val))
    (null?-exp (arg)
      (bool-val (emptylist? (value-of arg nenv))))
    (cons-exp (car cdr)
      (pair-val
        (value-of car nenv)
        (value-of cdr nenv)))
    (car-exp (lst-exp)
      (pair-car (value-of lst-exp nenv)))
    (cdr-exp (lst-exp)
      (pair-cdr (value-of lst-exp nenv)))
    (list-exp (exprs)
      (list-val (map (curryr value-of nenv) exprs)))
    (nameless-unpack-exp (size lst body)
      (eval-unpack size lst body nenv))
    (else
      (eopl:error 'value-of "Cannot evaluate ~a" expr))))
