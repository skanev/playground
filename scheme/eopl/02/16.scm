; EOPL exercise 2.16
;
; Modify the implementation to use a representation in which there are no
; parentheses around the bound variable in a lambda expression.

(define (var-exp var) var)
(define (var-exp? exp) (symbol? exp))
(define (var-exp->var exp) exp)

(define (lambda-exp var body) (list 'lambda var body))
(define (lambda-exp? exp) (and (pair? exp) (eqv? (car exp) 'lambda)))
(define (lambda-exp->bound-var exp) (cadr exp))
(define (lambda-exp->body exp) (caddr exp))

(define (app-exp rator rand) (list rator rand))
(define (app-exp? exp) (and (pair? exp) (not (eqv? (car exp) 'lambda))))
(define (app-exp->rator exp) (car exp))
(define (app-exp->rand exp) (cadr exp))
