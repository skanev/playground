(load-relative "syntax.scm")
(load-relative "operations.scm")
(load-relative "compiler.scm")
(load-relative "explicit-evaluator-text.scm")
(load-relative "../simulator/simulator.scm")
(load-relative "../explicit/controller-text.scm")

(define (make-explicit+compile-machine)
  (make-machine ec-registers cm-operations explicit+compile-text))

(define (compile-in-machine machine expression)
  (let ((instructions (assemble (statements (compile-exp expression 'val 'return))
                                machine)))
    (set-register-contents! machine 'env the-global-environment)
    (set-register-contents! machine 'val instructions)
    (set-register-contents! machine 'flag true)

    (start machine)))

(define (eval-in-machine machine expression)
  (set-register-contents! machine 'env the-global-environment)
  (set-register-contents! machine 'exp expression)
  (set-register-contents! machine 'flag false)

  (start machine))

(define (compiled-instructions expression)
  (statements (compile-exp expression 'val 'return)))

