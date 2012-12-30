; The usual syntax functions.
;
; It's noteworthy that lambda body are scanning out defines.

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (quoted? exp)
  (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (let ((names (map car (cadr exp)))
        (values (map cadr (cadr exp)))
        (body (cddr exp)))
    (cons (cons 'lambda (cons names body))
          values)))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (scan-out-defines (cddr exp)))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last - COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (scan-out-defines body)
  (define (definitions-in body)
    (cond ((null? body)
           '())
          ((definition? (car body))
           (cons (car body) (definitions-in (cdr body))))
          (else
           (definitions-in (cdr body)))))
  (define (body-without-definitions body)
    (cond ((null? body)
           '())
          ((definition? (car body))
           (body-without-definitions (cdr body)))
          (else
           (cons (car body) (body-without-definitions (cdr body))))))
  (define (definition->unassigned-pair definition)
    (list (definition-variable definition) ''*unassigned*))
  (define (definition->set! definition)
    (list 'set! (definition-variable definition) (definition-value definition)))
  (define (defines->let definitions body)
    (list
      (cons 'let
            (cons (map definition->unassigned-pair definitions)
                  (append (map definition->set! definitions) body)))))

  (let ((internal-definitions (definitions-in body)))
    (if (null? internal-definitions)
        body
        (defines->let internal-definitions (body-without-definitions body)))))
