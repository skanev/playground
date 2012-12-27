(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (list->mlist lst)
  (if (null? lst)
      '()
      (mcons (car lst) (list->mlist (cdr lst)))))

(define (text-of-quotation exp) (cadr exp))


(define (last-operand? ops) (null? (cdr ops)))

(define (make-procedure parameters body env) (list 'procedure parameters body env))
(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))

(define (true? x) (not (eq? x false)))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))
(define (frame-variables frame) (mcar frame))
(define (frame-values frame) (mcdr frame))
(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (cons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars (list->mlist vals)) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
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
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
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
             (set-mcar! vals val))
            (else (scan (cdr vars) (mcdr vals)))))
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
        (list '> >)
        (list '< <)
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
(define (get-global-environment) the-global-environment)

(define (reset-the-global-environment!)
  (set! the-global-environment (setup-environment)))

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

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

; The list of registers

(define cm-registers '(exp env val proc argl continue unev))

; The list of all operations

(define cm-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)

        (list 'quoted? quoted?)
        (list 'text-of-quotation text-of-quotation)

        (list 'assignment? assignment?)
        (list 'assignment-value assignment-value)
        (list 'assignment-variable assignment-variable)

        (list 'definition? definition?)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)

        (list 'if? if?)
        (list 'if-predicate if-predicate)
        (list 'if-consequent if-consequent)
        (list 'if-alternative if-alternative)

        (list 'lambda? lambda?)
        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)

        (list 'begin? begin?)
        (list 'begin-actions begin-actions)
        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)

        (list 'application? application?)
        (list 'operands operands)
        (list 'operator operator)
        (list 'no-operands? no-operands?)
        (list 'last-operand? last-operand?)
        (list 'first-operand first-operand)
        (list 'rest-operands rest-operands)

        (list 'lookup-variable-value lookup-variable-value)
        (list 'define-variable! define-variable!)
        (list 'set-variable-value! set-variable-value!)
        (list 'extend-environment extend-environment)

        (list 'primitive-procedure? primitive-procedure?)
        (list 'apply-primitive-procedure apply-primitive-procedure)

        (list 'compound-procedure? compound-procedure?)
        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-body procedure-body)

        (list 'make-procedure make-procedure)

        (list 'empty-arglist empty-arglist)
        (list 'adjoin-arg adjoin-arg)

        (list 'true? true?)
        (list 'false? false?)

        (list 'list list)
        (list 'cons cons)

        (list 'compiled-procedure? compiled-procedure?)
        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure-env compiled-procedure-env)
        (list 'compiled-procedure-entry compiled-procedure-entry)

        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'get-global-environment get-global-environment)
        (list 'announce-output announce-output)
        (list 'user-print user-print)

        (list 'error error)))
