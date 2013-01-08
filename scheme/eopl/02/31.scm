; EOPL exercise 2.31
;
; Sometimes it is useful to specify a concrete syntax as a sequence of symbols
; and integers, surrounded by parentheses. For example, one might define the
; set of prefix lists by
;
;   Prefix-list ::= (Prefix-exp)
;   Prefix-exp  ::= Int
;               ::= - Prefix-exp Prefix-exp
;
; so that (- - 3 2 - 4 - 12 7) is a legal prefix list. This is sometimes
; called Polish prefix notation, after its inventor, Jan Łukasiewicz. Write a
; parser to convert a prefix-list of the abstract syntax
;
; (define-datatype prefix-exp prefix-exp?
;   (const-exp
;     (num integer?))
;   (diff-exp
;     (operand1 prefix-exp?)
;     (operand2 prefix-exp?)))
;
; so that the example above produces the same abstract syntax tree as the
; sequence of constructors
;
; (diff-exp
;   (diff-exp
;     (const-exp 3)
;     (const-exp 2))
;   (diff-exp
;     (const-exp 4)
;     (diff-exp
;       (const-exp 12)
;       (const-exp 7))))
;
; As a hit, consider writing a procedure that takes a list and produces a
; prefix-exp and the list of leftover list elements.

(define-datatype prefix-exp prefix-exp?
  (const-exp
    (num integer?))
  (diff-exp
    (operand1 prefix-exp?)
    (operand2 prefix-exp?)))

(define (read-prefix-exp datum)
  (cond ((null? datum)
         (eopl:error 'parse "Unexpected end of input"))
        ((number? (car datum))
         (cons (const-exp (car datum))
               (cdr datum)))
        ((eqv? (car datum) '-)
         (let* ((result-1 (read-prefix-exp (cdr datum)))
                (operand-1 (car result-1))
                (suffix-1 (cdr result-1))
                (result-2 (read-prefix-exp suffix-1))
                (operand-2 (car result-2))
                (suffix-2 (cdr result-2)))
           (cons (diff-exp operand-1 operand-2)
                 suffix-2)))
        (else
         (eopl:error 'read-prefix-exp "Unrecognized input: ~s" (car datum)))))

(define (parse datum)
  (let* ((result (read-prefix-exp datum))
         (prefix-exp (car result))
         (remaining (cdr result)))
    (if (null? remaining)
        prefix-exp
        (eopl:error 'parse "Trailing output: ~s" remaining))))
