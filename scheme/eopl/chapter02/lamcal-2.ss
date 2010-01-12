; Exercise 2.16
(module lamcal-2 eopl
  (provide var-exp var-exp? var-exp->var
           lambda-exp lambda-exp? lambda-exp->bound-var lambda-exp->body
           app-exp app-exp? app-exp->rator app-exp->rand)

  (define var-exp (lambda (var) var))
  (define var-exp? (lambda (var) (symbol? var)))
  (define var-exp->var (lambda (var-exp) var-exp))

  (define lambda-exp (lambda (var lexp) (list 'lambda var lexp)))
  (define lambda-exp? (lambda (expr) (and (list? expr) (eqv? (car expr) 'lambda))))
  (define lambda-exp->bound-var (lambda (lexp) (cadr lexp)))
  (define lambda-exp->body (lambda (lexp) (caddr lexp)))

  (define app-exp (lambda (rator rand) (list rator rand)))
  (define app-exp? (lambda (expr) (and (list? expr) (not (eqv? (car expr) 'lambda)))))
  (define app-exp->rator (lambda (aexp) (car aexp)))
  (define app-exp->rand (lambda (aexp) (cadr aexp)))
)
