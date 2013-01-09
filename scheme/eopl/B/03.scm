; EOPL exercise B.03
;
; Define an interpreter that takes the syntax tree produced by the parser of
; exercise B.1 and evaluates it as an arithmetic expression. The parser takes
; care of the usual arithmetic precendence operations, but the interpreter
; will have to take care of associativity, that is, making sure that the
; operations at the same precendence level (e.g. additions and subtractions)
; are performed from left to right. Since there are no variables in these
; expressions, this interpreter need not take an environment parameter.

(load-relative "01.scm")

(define (eval* tree)
  (cases ast tree
    (op (first ops rest)
      (apply-ops (eval* first) ops (map eval* rest)))
    (number (val)
      val)
    (factor (expr)
      (eval* expr))))

(define (apply-ops first ops rest)
  (if (null? ops)
      first
      (apply-ops ((eval (car ops)) first (car rest))
                 (cdr ops)
                 (cdr rest))))

(define (value-of code)
  ((compose eval* scan&parse) code))
