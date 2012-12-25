; SICP exercise 5.30
;
; Our evaluator currently catches and signals only two kind of errors --
; unknown expression types and unknown procedure types. Other errors will take
; us out of the evaluator read-eval-print-loop. When we run the evaluator
; using the register machine simulator, these errors are caught by the
; underlying Scheme system. It is a large project to make a real error system
; work, but it is well worth the effort to understand what is involved here.
;
; a. Errors that occur in the evaluation process, such as an attempt to access
; an unbound variable, could be caught by changing the lookup operation to
; make it return a distinguished condition code, which cannot be a possible
; value of any user variable. The evaluator can test for this condition code
; and then do what is necessary to signal error. Find all the places in the
; evaluator where such a change is necessary and fix them. This is lots of
; work.
;
; b. Much worse is the problem of handling errors that are signaled by
; applying primitive procedures, such as an attempt to divide by zero or an
; attempt to extract the car of a symbol. In a professionally written
; high-quality system, each primitive application is checked for safety as
; part of the primitive. For example, every call to car could first check that
; the argument is a pair. If the argument is not a pair, the application would
; return a distinguished condition code to the evaluator, which would then
; report the failure. We could arrange for this in our register-machine
; simulator by making each primitive procedure check of applicability and
; returning an appropriate distinguished condition code on failure. Then the
; primitive-apply code in the evaluator can check for the condition code and
; go to signal-error in necessary. Build this structure and make it work. This
; is a major project.

; You know, I'm going to do both on the same pass.
;
; I am going to change things a bit. First, since I don't want to test in the
; REPL, I will have the machine return the error code itself in val when an
; error has occured. Second, I'm going to ignore unrolling the stack -- this
; topic is not covered in the book anyway.
;
; Also, I'm only going to implement the examples given. I could implement a
; few other errors (bad function call arity, setting an undefined variable,
; redefining a variable, etc.), but I don't think I need to.
;
; That way, it actually isn't that much work.

; Error codes

(define (error-code code debug) (vector 'error code debug))
(define (error-code? obj) (and (vector? obj) (eq? (vector-ref obj 0) 'error)))
(define (error-code-sym code) (vector-ref code 1))
(define (error-code-debug code) (vector-ref code 2))

; The new operations required

(define (lookup-variable-value-e var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (mcar vals))
            (else (scan (cdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error-code 'unbound-variable var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

; New versions of the primitive procedures

(define (car-e lst)
  (cond ((null? lst) (error-code 'car-on-null '()))
        ((not (pair? lst)) (error-code 'car-on-non-pair lst))
        (else (car lst))))

(define (/-e a b)
  (if (zero? b)
      (error-code 'zero-division-error a)
      (/ a b)))

(define primitive-procedures
  (list (list 'car car-e)
        (list '/ /-e)
        (list '+ +)))

; Additional operations

(define extra-operations
  (list (list 'lookup-variable-value-e lookup-variable-value-e)
        (list 'error-code? error-code?)
        (list 'error-code-sym error-code-sym)))

; The controller text

(define ec-error-support
  '(
      (assign continue (label done))
    eval-dispatch
      (test (op self-evaluating?) (reg exp))
      (branch (label ev-self-eval))
      (test (op variable?) (reg exp))
      (branch (label ev-variable))
      (test (op quoted?) (reg exp))
      (branch (label ev-quoted))
      (test (op assignment?) (reg exp))
      (branch (label ev-assignment))
      (test (op definition?) (reg exp))
      (branch (label ev-definition))
      (test (op if?) (reg exp))
      (branch (label ev-if))
      (test (op lambda?) (reg exp))
      (branch (label ev-lambda))
      (test (op begin?) (reg exp))
      (branch (label ev-begin))
      (test (op application?) (reg exp))
      (branch (label ev-application))
      (goto (label unknown-expression-type))

    ; Evaluating simple expressions

    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))
    ev-variable
      (assign val (op lookup-variable-value-e) (reg exp) (reg env))
      (test (op error-code?) (reg val))
      (branch (label undefined-variable))
      (goto (reg continue))
    ev-quoted
      (assign val (op text-of-quotation) (reg exp))
      (goto (reg continue))
    ev-lambda
      (assign unev (op lambda-parameters) (reg exp))
      (assign exp (op lambda-body) (reg exp))
      (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
      (goto (reg continue))

    ; Evaluating procedure applications

    ev-application
      (save continue)
      (save env)
      (assign unev (op operands) (reg exp))
      (save unev)
      (assign exp (op operator) (reg exp))
      (assign continue (label ev-appl-did-operator))
      (goto (label eval-dispatch))
    ev-appl-did-operator
      (restore unev)
      (restore env)
      (assign argl (op empty-arglist))
      (assign proc (reg val))
      (test (op no-operands?) (reg unev))
      (branch (label apply-dispatch))
      (save proc)
    ev-appl-operand-loop
      (save argl)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-appl-last-arg))
      (save env)
      (save unev)
      (assign continue (label ev-appl-accumulate-arg))
      (goto (label eval-dispatch))
    ev-appl-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-appl-operand-loop))
    ev-appl-last-arg
      (assign continue (label ev-appl-accum-last-arg))
      (goto (label eval-dispatch))
    ev-appl-accum-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (goto (label apply-dispatch))

    ; Procedure application

    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (test (op error-code?) (reg val))
      (branch (label primitive-apply-error))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

    ; Sequence evaluation

    ev-begin
      (assign unev (op begin-actions) (reg exp))
      (save continue)
      (goto (label ev-sequence))
    ev-sequence
      (assign exp (op first-exp) (reg unev))
      (test (op last-exp?) (reg unev))
      (branch (label ev-sequence-last-exp))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-last-exp
      (restore continue)
      (goto (label eval-dispatch))

    ; Conditionals

    ev-if
      (save exp)
      (save env)
      (save continue)
      (assign continue (label ev-if-decide))
      (assign exp (op if-predicate) (reg exp))
      (goto (label eval-dispatch))
    ev-if-decide
      (restore continue)
      (restore env)
      (restore exp)
      (test (op true?) (reg val))
      (branch (label ev-if-consequent))
    ev-if-alternative
      (assign exp (op if-alternative) (reg exp))
      (goto (label eval-dispatch))
    ev-if-consequent
      (assign exp (op if-consequent) (reg exp))
      (goto (label eval-dispatch))

    ; Assignments and definitions

    ev-assignment
      (assign unev (op assignment-variable) (reg exp))
      (save unev)
      (assign exp (op assignment-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-assignment-1))
      (goto (label eval-dispatch))
    ev-assignment-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))

    ev-definition
      (assign unev (op definition-variable) (reg exp))
      (save unev)
      (assign exp (op definition-value) (reg exp))
      (save env)
      (save continue)
      (assign continue (label ev-definition-1))
      (goto (label eval-dispatch))
    ev-definition-1
      (restore continue)
      (restore env)
      (restore unev)
      (perform (op define-variable!) (reg unev) (reg val) (reg env))
      (assign val (const ok))
      (goto (reg continue))
    undefined-variable
      (assign val (op error-code-sym) (reg val))
      (goto (label done))
    primitive-apply-error
      (assign val (op error-code-sym) (reg val))
      (goto (label done))

    unknown-procedure-type
    unknown-expression-type
    done))
