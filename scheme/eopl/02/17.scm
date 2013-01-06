; EOPL exercise 2.17
;
; Invent at least two other representations of the data type of
; lambda-calculus expressions and implement them.

(define var-exp (void))
(define var-exp? (void))
(define var-exp->var (void))

(define lambda-exp (void))
(define lambda-exp? (void))
(define lambda-exp->bound-var (void))
(define lambda-exp->body (void))

(define app-exp (void))
(define app-exp? (void))
(define app-exp->rator (void))
(define app-exp->rand (void))

; 1. Lc-exp ::= Identifier
;           ::= (Identifier -> Lc-exp)
;           ::= (Lc-exp (Lc-exp))

(define (use-representation-1)
  (set! var-exp (lambda (var) var))
  (set! var-exp? symbol?)
  (set! var-exp->var (lambda (var-exp) var-exp))

  (set! lambda-exp (lambda (var body) (list var '-> body)))
  (set! lambda-exp? (lambda (exp) (and (pair? exp)
                                       (pair? (cdr exp))
                                       (eqv? (cadr exp) '->))))
  (set! lambda-exp->bound-var car)
  (set! lambda-exp->body caddr)

  (set! app-exp (lambda (rator rand) `(,rator (,rand))))
  (set! app-exp? (lambda (exp) (and (pair? exp)
                                    (pair? (cdr exp))
                                    (pair? (cadr exp)))))
  (set! app-exp->rator car)
  (set! app-exp->rand caadr))

; 2. Lc-exp ::= (var-exp Identifier)
;           ::= (lambda-exp Identifier Lc-exp)
;           ::= (app-exp Lc-exp Lc-exp)

(define (use-representation-2)
  (define (tagged-list? exp tag) (and (pair? exp)
                                      (eqv? (car exp) tag)))

  (set! var-exp (curry list 'var-exp))
  (set! var-exp? (curryr tagged-list? 'var-exp))
  (set! var-exp->var cadr)

  (set! lambda-exp (curry list 'lambda-exp))
  (set! lambda-exp? (curryr tagged-list? 'lambda-exp))
  (set! lambda-exp->bound-var cadr)
  (set! lambda-exp->body caddr)

  (set! app-exp (curry list 'app-exp))
  (set! app-exp? (curryr tagged-list? 'app-exp))
  (set! app-exp->rator cadr)
  (set! app-exp->rand caddr))
