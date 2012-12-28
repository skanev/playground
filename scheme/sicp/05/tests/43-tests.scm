(require rackunit rackunit/text-ui)
(load "helpers/compiler.scm")
(load "../43.scm")

(define (run exp)
  (let ((machine (make-explicit+compile-machine)))
    (compile-in-machine machine exp)
    (get-register-contents machine 'val)))

(define sicp-5.43-tests
  (test-suite
    "Tests for SICP exercise 5.43"

    (check-equal? (run '(begin (define (foo)
                                 (define x 1)
                                 (set! x 2)
                                 x)
                               (foo)))
                  2)
))

(run-tests sicp-5.43-tests)
