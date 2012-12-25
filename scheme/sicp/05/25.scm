; SICP exercise 5.25
;
; Modify the evaluator so that it uses normal-order evaluation, based on the
; lazy evaluator of section 4.2.

; This is tricky.
;
; First, in order to do normal-order evaluation, we should not cache thunk
; values. That would beat the purpose of normal-order.
;
; Second, we will accomplish this with a more general procedure called
; ev-map-operands, that will apply the procedure in the exp register to each
; argument in unev. It is used by primitive-apply with actual-value and
; compound-apply with delay-it.
;
; We will also modify ev-if to use actual-value.

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define extra-operations
  (list (list 'delay-it delay-it)
        (list 'thunk? thunk?)
        (list 'thunk-env thunk-env)
        (list 'thunk-exp thunk-exp)))

(define ec-core
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

    ; Delaying expressions

    delay-it
      (assign val (op delay-it) (reg exp) (reg env))
      (goto (reg continue))

    actual-value
      (save continue)
      (assign continue (label actual-value-after-eval))
      (goto (label eval-dispatch))
    actual-value-after-eval
      (restore continue)
      (assign exp (reg val))
      (goto (label force-it))

    force-it
      (test (op thunk?) (reg exp))
      (branch (label force-it-thunk))
      (goto (reg continue))
    force-it-thunk
      (assign env (op thunk-env) (reg exp))
      (assign exp (op thunk-exp) (reg exp))
      (goto (label actual-value))

    ; Evaluating simple expressions

    ev-self-eval
      (assign val (reg exp))
      (goto (reg continue))
    ev-variable
      (assign val (op lookup-variable-value) (reg exp) (reg env))
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
      (assign proc (reg val))
      (goto (label apply-dispatch))

    ; Procedure application

    apply-dispatch
      (test (op primitive-procedure?) (reg proc))
      (branch (label primitive-apply))
      (test (op compound-procedure?) (reg proc))
      (branch (label compound-apply))
      (goto (label unknown-procedure-type))
    primitive-apply
      (assign exp (label actual-value))
      (assign continue (label primitive-apply-after-args))
      (goto (label ev-map-operands))
    primitive-apply-after-args
      (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      (restore continue)
      (goto (reg continue))
    compound-apply
      (save continue)
      (assign exp (label delay-it))
      (assign continue (label compound-apply-after-args))
      (goto (label ev-map-operands))
    compound-apply-after-args
      (restore continue)
      (assign unev (op procedure-parameters) (reg proc))
      (assign env (op procedure-environment) (reg proc))
      (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
      (assign unev (op procedure-body) (reg proc))
      (goto (label ev-sequence))

    ev-map-operands
      (assign argl (op empty-arglist))
      (test (op no-operands?) (reg unev))
      (branch (label ev-map-no-args))
      (save continue)
      (save proc)
      (assign proc (reg exp))
      (save proc)
    ev-map-operand-loop
      (restore proc)
      (assign exp (op first-operand) (reg unev))
      (test (op last-operand?) (reg unev))
      (branch (label ev-map-last-arg))
      (save proc)
      (save argl)
      (save env)
      (save unev)
      (assign continue (label ev-map-accumulate-arg))
      (goto (reg proc))
    ev-map-accumulate-arg
      (restore unev)
      (restore env)
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (assign unev (op rest-operands) (reg unev))
      (goto (label ev-map-operand-loop))
    ev-map-last-arg
      (save argl)
      (assign continue (label ev-map-accumulate-last-arg))
      (goto (reg proc))
    ev-map-accumulate-last-arg
      (restore argl)
      (assign argl (op adjoin-arg) (reg val) (reg argl))
      (restore proc)
      (restore continue)
      (goto (reg continue))
    ev-map-no-args
      (goto (reg continue))

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
      (goto (label actual-value))
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
    unknown-expression-type
    unknown-procedure-type
    done))
