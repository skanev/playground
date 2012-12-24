(load-relative "../../showcase/explicit/evaluator.scm")

(define (run-controller controller-text additional-operations exp)
  (let ((machine (make-machine ec-registers
                               (append ec-operations
                                       additional-operations)
                               controller-text)))
    (set-register-contents! machine 'exp exp)
    (set-register-contents! machine 'env (setup-environment))

    (start machine)

    (get-register-contents machine 'val)))
