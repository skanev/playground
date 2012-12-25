; SICP exercise 5.28
;
; Modify the definition of the evaluator by changing eval-sequence as
; described in section 5.4.2 so that the evaluator is no longer
; tail-recursive. Rerun your experiments from exercise 5.26 to 5.27 to
; demonstrate that both versions of the factorial procedure now require space
; that grows linearly with their input.

; The results are:
;
; Iterative factorial:
;   1! takes (total-pushes = 70 maximum-depth = 17)
;   2! takes (total-pushes = 107 maximum-depth = 20)
;   3! takes (total-pushes = 144 maximum-depth = 23)
;   4! takes (total-pushes = 181 maximum-depth = 26)
;   5! takes (total-pushes = 218 maximum-depth = 29)
;   6! takes (total-pushes = 255 maximum-depth = 32)
;   7! takes (total-pushes = 292 maximum-depth = 35)
;   8! takes (total-pushes = 329 maximum-depth = 38)
;   9! takes (total-pushes = 366 maximum-depth = 41)
; Recursive factorial:
;   1! takes (total-pushes = 18 maximum-depth = 11)
;   2! takes (total-pushes = 52 maximum-depth = 19)
;   3! takes (total-pushes = 86 maximum-depth = 27)
;   4! takes (total-pushes = 120 maximum-depth = 35)
;   5! takes (total-pushes = 154 maximum-depth = 43)
;   6! takes (total-pushes = 188 maximum-depth = 51)
;   7! takes (total-pushes = 222 maximum-depth = 59)
;   8! takes (total-pushes = 256 maximum-depth = 67)
;   9! takes (total-pushes = 290 maximum-depth = 75)
;
; One can see that both versions are not bound on stack space. The code to
; reproduce the results is below:

(load-relative "tests/helpers/evaluator.scm")
(load-relative "tests/helpers/monitored-stack.scm")

(define ec-no-tail
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
      (test (op no-more-exps?) (reg unev))
      (branch (label ev-sequence-end))
      (assign exp (op first-exp) (reg unev))
      (save unev)
      (save env)
      (assign continue (label ev-sequence-continue))
      (goto (label eval-dispatch))
    ev-sequence-continue
      (restore env)
      (restore unev)
      (assign unev (op rest-exps) (reg unev))
      (goto (label ev-sequence))
    ev-sequence-end
      (restore continue)
      (goto (reg continue))

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
    unknown-procedure-type
    unknown-expression-type
    done))

(define iterative-code
  '(define (factorial-iterative n)
     (define (iter product counter)
       (if (> counter n)
         product
         (iter (* counter product)
               (+ counter 1))))
     (iter 1 1)))

(define recursive-code
  '(define (factorial-recursive n)
     (if (= n 1)
       1
       (* (factorial-recursive (- n 1)) n))))

(define machine (make-explicit-machine ec-no-tail `((no-more-exps? ,null?))))

(set-register-contents! machine 'env the-global-environment)

(set-register-contents! machine 'exp iterative-code)
(start machine)

(set-register-contents! machine 'exp recursive-code)
(start machine)

(define (print-stats-for function name)
  (printf "~a:\n" name)
  (for ([n (in-range 1 10)])
    (printf "  ~a! takes ~a\n" n (stack-stats-for machine (list function n)))))

(print-stats-for 'factorial-iterative "Iterative factorial")
(print-stats-for 'factorial-recursive "Recursive factorial")
