; EOPL exercise 2.29
;
; Where a Kleene star or plus (page 7) is used in concrete syntax, it is most
; convenient to use a list of associated subtrees when constructing an
; abstract syntax tree. For example, if the grammar for lambda-calculus
; expressions had been
;
;   Lc-exp ::= Identifier
;              +---------------+
;              | var-exp (var) |
;              +---------------+
;
;          ::= (lambda ({Identifier}*) Lc-exp)
;              +------------------------------+
;              | lambda-exp (bound-vars body) |
;              +------------------------------+
;
;          ::= (Lc-exp {Lc-exp}*)
;              +-----------------------+
;              | app-exp (rator rands) |
;              +-----------------------+
;
; then the predicate for the bound-vars field could be (list-of identifier?),
; and the predicate for the rands fields could be (list-of lc-exp?). Write a
; define-datatype and a parser for this grammar that works in this way.

(define (id? sym)
  (and (symbol? sym)
       (not (eqv? sym 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var id?))
  (lambda-exp
    (args (list-of id?))
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))))

(define (parse sexp)
  (cond ((eqv? sexp 'lambda)
         (eopl:error 'parse "lambda is not a valid identifier"))
        ((symbol? sexp)
         (var-exp sexp))
        ((and (pair? sexp)
              (eqv? (car sexp) 'lambda))
         (lambda-exp (cadr sexp) (parse (caddr sexp))))
        ((and (pair? sexp))
         (app-exp (parse (car sexp))
                  (map parse (cdr sexp))))
        (else
         (eopl:error 'parse "Don't know how to parse ~s" sexp))))
