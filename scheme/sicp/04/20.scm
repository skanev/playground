; SICP exercise 4.20
;
; Because internal definitions look sequential but are actually simultaneous,
; some people prefer to avoid them entirely, and use the special form letrec
; instead. Letrec looks like let, so it is no surprising that the variables it
; binds are bound simultaneously and have the same scope as each other. The
; sample procedure f above can be written without internal definitions, but
; with exactly the same meaning, as:
;
;   (define (f x)
;     (letrec ((even?
;                (lambda (n)
;                  (if (= n 0)
;                      true
;                      (odd? (- n 1)))))
;              (odd?
;                (lambda (n)
;                  (if (= n 0)
;                      false
;                      (even? (- n 1))))))
;       <rest-of-body-of-f>))
;
; letrec expressions, which have the form
;
;   (letrec ((<var-1> <exp-1>) ... (<var-n> <exp-n>))
;     <body>)
;
; are a variation on let in which the expressions <exp-k> that provide the
; initial values for the variables <var-k> are evaluated in an environment
; that includes all the letrec bindings. This permits recursion in the
; bindings, such as the mutual recursion of even? and odd? in the example
; above, or the evaluation of 10 factorial with
;
;   (letrec ((fact
;              (lambda (n)
;                (if (= n 1)
;                    1
;                    (* n (fact (- n 1)))))))
;     (fact 10))
;
; a. Implement letrec as a derived expression, by transforming a letrec
; expression into a let expression as shown in the text above or in exercise
; 4.18. That is, the letrec variables should be created with a let and then be
; assigned their values with set!.
;
; b. Louis Reasoner is confused by all this fuss about internal definitions.
; The way he sees it, if you don't like to use define inside a procedure, you
; can just use let. Illustrate what is loose about his reasoning by drawing an
; environment diagram that shows the environment in which <rest-of-body-of-f>
; is evaluated during evaluation of the expression (f 5), with f defined as in
; this exercise. Draw an environment diagram for the same evaluation, but with
; let in place of letrec in the definition of f.

(require r5rs/init)

; a. Let's begin with the letrec implementation:

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-pairs exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec->combination exp)
  (cons 'let
        (cons (map (lambda (pair) (list (car pair) ''*unassigned*))
                   (letrec-pairs exp))
              (append (map (lambda (pair) (list 'set!
                                                (car pair)
                                                (cadr pair)))
                           (letrec-pairs exp))
                      (letrec-body exp)))))

; It is installed in the evaluator.
;
; b. This is the code Louis proposes:
;
;   (define (f x)
;     (let ((even?
;             (lambda (n)
;               (if (= n 0)
;                   true
;                   (odd? (- n 1)))))
;           (odd?
;             (lambda (n)
;               (if (= n 0)
;                   false
;                   (even? (- n 1))))))
;       <rest-of-body-of-f>))
;
; Here's the environment diagram of (f 10):
;
; f: +-------------------+
;    | x: 10             |
;    +-------------------+
;            ^ ^ ^
;            | | |
;            | | +---------------------------------------+
;            | +----------------------+                  |
;    +-------------------+            |            +----------+
;    | even?: <lambda> ---------------|------------| <lambda> |
;    | odd?: <lambda>  -----+         |            +----------+
;    +-------------------+  |   +----------+        params: n
;              ^            +---| <lambda> |        code: (if (= n 0)
;              |                +----------+                  true
;     <rest-of-body-of-f>        params: n                    (odd? (- n 1)))
;                                code: (if (= n 0)
;                                          false
;                                          (even? (- n 1)))
;
; It is apparant that even? is not defined in odd?'s environment and odd? is
; not defined in even?'s environment. Thus, it will simply not work.

; The rest of the evaluator:

(define (evaluate exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (evaluate (cond->if exp) env))
        ((let? exp) (evaluate (let->combination exp) env))
        ((letrec? exp) (evaluate (letrec->combination exp) env))
        ((application? exp)
         (apply-procedure (evaluate (operator exp) env)
                          (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type - EVALUATE" exp))))

(define (apply-procedure procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type - APPLY-PROCEDURE" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaluate (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (evaluate (if-predicate exp) env))
      (evaluate (if-consequent exp) env)
      (evaluate (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaluate (first-exp exps) env))
        (else (evaluate (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (evaluate (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (evaluate (definition-value exp) env)
                    env)
  'ok)

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (let? exp) (tagged-list? exp 'let))
(define (let->combination exp)
  (let ((names (map car (cadr exp)))
        (values (map cadr (cadr exp)))
        (body (cddr exp)))
    (cons (cons 'lambda (cons names body))
          values)))


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

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable - SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'list list)
        (list '= =)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))
(define the-global-environment (setup-environment))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (evaluate input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
