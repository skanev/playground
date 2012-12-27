(load-relative "../../showcase/compiler/helpers.scm")

(define (run-compiler-with-text controller-text extra-operations exp)
  (let ((machine (make-machine ec-registers
                               (append cm-operations extra-operations)
                               controller-text)))
    (compile-in-machine machine exp)
    (get-register-contents machine 'val)))
