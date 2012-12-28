; SICP exercise 5.49
;
; As an alternative to using the explicit-control evaluator's read-eval-print
; loop, design a register machine that performs a read-compile-execute-print
; loop. That is, the machine should run a loop that reads an expression,
; compiles it, assembles and executes the resulting code, and prints the
; result. This is easy to run in our simulated setup, wince we can arrange to
; call the procedures compile and assemble as "register-machine operations".

(load-relative "showcase/compiler/helpers.scm")

(define (compile-in-machine exp)
  (assemble (statements (compile-exp exp 'val 'return)) eceval))

(define cm-operations
  (append cm-operations `((compile ,compile-in-machine))))

(define explicit+compile-text
  '(read-eval-print-loop
      (perform (op initialize-stack))
      (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
      (assign exp (op read))
      (assign env (op get-global-environment))
      (assign continue (label print-result))
      (assign val (op compile) (reg exp))
      (goto (reg val))
    print-result
      (perform (op announce-output) (const ";;; EC-Eval value:"))
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    unknown-expression-type
      (assign val (const unknown-expression-type-error))
      (goto (label signal-error))
    unknown-procedure-type
      (restore continue)
      (assign val (const unknown-procedure-type-error))
      (goto (label signal-error))
    signal-error
      (perform (op user-print) (reg val))
      (goto (label read-eval-print-loop))
    unknown-expression-type
      (perform (op error) (const "Unknown expression type") (reg exp))
    unknown-procedure-type
      (perform (op error) (const "Unknown procedure type") (reg proc))))

(define eceval (make-explicit+compile-machine))
(set-register-contents! eceval 'env the-global-environment)

(start eceval)
