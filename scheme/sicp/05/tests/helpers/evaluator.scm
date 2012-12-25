(load-relative "../../showcase/explicit/evaluator.scm")

(define (make-explicit-machine controller-text additional-operations)
  (make-machine ec-registers
                (append ec-operations additional-operations)
                controller-text))

(define (run-controller controller-text additional-operations exp)
  (let ((machine (make-explicit-machine controller-text additional-operations)))
    (set-register-contents! machine 'exp exp)
    (set-register-contents! machine 'env (setup-environment))

    (start machine)

    (get-register-contents machine 'val)))
