; SICP exercise 4.03
;
; Rewrite eval so that the dispatch is done in data-directed style. Compare
; this with the data-directed procedure of exercise 2.73. (You may use the car
; of a compound expression as the type of the expression, as is appropriate
; for the syntax implemented in this section.)

; That should be fun.
;
; I'm going to define a special-from? predicate and an evaluate-form
; procedure. Both will look up in the table and see what to do.
;
; I might end up using this interpreter for some of the future exercises.
;
; As for the comparison with 2.73, it is nice that we can now add new special
; forms without modifying eval. This comes with the assumption that every
; special form should be identifiable by the first symbol in its s-exp, but I
; believe that to hold for all of LISP.
;
; The modified evaluator is below:

(require r5rs/init)

; First we start with eval and apply.

(define (evaluate exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((special-form? exp) (evaluate-form exp env))
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

; Here is the data-directed infrastructure for the form:

(define special-forms (make-hash))

(define (define-form keyword handler)
  (hash-set! special-forms keyword handler))

(define (get-form keyword)
  (hash-ref special-forms keyword))

(define (form-defined? keyword)
  (if (hash-ref special-forms keyword false) true false))

; Here are the mentioned procedures

(define (special-form? exp)
  (and (pair? exp)
       (form-defined? (car exp))))

(define (evaluate-form exp env)
  ((get-form (car exp)) (cdr exp) env))

; And here is the definitions of the existing forms:

(define-form 'quote (lambda (exp env) (car exp)))

(define-form 'if
  (lambda (exp env)
    (if (true? (evaluate (car exp) env))
        (evaluate (cadr exp) env)
        (if (null? (cddr exp))
            false
            (evaluate (caddr exp) env)))))

(define-form 'set!
  (lambda (exp env)
    (let ((name (car exp))
          (value (evaluate (cadr exp) env)))
      (set-variable-value! name value env)
      'ok)))

(define-form 'define
  (lambda (exp env)
    (let ((name (if (symbol? (car exp))
                    (car exp)
                    (caar exp)))
          (value (if (symbol? (car exp))
                     (cadr exp)
                     (make-lambda (cdar exp) (cdr exp)))))
      (define-variable! name (evaluate value env) env)
      'ok)))

(define-form 'lambda
  (lambda (exp env)
    (make-procedure (car exp) (cdr exp) env)))

(define-form 'begin
  (lambda (exp env)
    (eval-sequence exp env)))

(define-form 'cond
  (lambda (exp env)
    (define clauses exp)
    (define (else-clause? clause)
      (eq? (predicate clause) 'else))
    (define (predicate clause) (car clause))
    (define (actions clause) (cdr clause))
    (define (expand-clauses clauses)
      (if (null? clauses)
          'false
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (if (else-clause? first)
                (if (null? rest)
                    (sequence->exp (actions first))
                    (error "ELSE clause isn't last - COND" clauses))
                (make-if (predicate first)
                         (sequence->exp (actions first))
                         (expand-clauses rest))))))
    (evaluate (expand-clauses exp) env)))

; And this is the rest of the interpreter

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (evaluate (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evaluate (first-exp exps) env))
        (else (evaluate (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (make-lambda parameters body) (cons 'lambda (cons parameters body)))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

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
