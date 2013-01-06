; EOPL exercise 2.15
;
; Implement the lambda-calculus interface for the representations specified by
; the grammar above.

(define (var-exp var) var)
(define (var-exp? exp) (symbol? exp))
(define (var-exp->var exp) exp)

(define (lambda-exp var body) `(lambda (,var) ,body))
(define (lambda-exp? exp) (and (pair? exp) (eqv? (car exp) 'lambda)))
(define (lambda-exp->bound-var exp) (caadr exp))
(define (lambda-exp->body exp) (caddr exp))

(define (app-exp rator rand) (list rator rand))
(define (app-exp? exp) (and (pair? exp) (not (eqv? (car exp) 'lambda))))
(define (app-exp->rator exp) (car exp))
(define (app-exp->rand exp) (cadr exp))
