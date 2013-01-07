(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../23.scm")

(define eopl-2.23-tests
  (test-suite
    "Tests for EOPL exercise 2.23"

    (check-exn exn? (lambda () (app-exp (var-exp 'lambda) (var-exp 'lambda))))
    (check-exn exn? (lambda () (app-exp (var-exp 'a) (var-exp 'lambda))))
    (check-exn exn? (lambda () (app-exp (var-exp 'lambda) (var-exp 'a))))
    (check-exn exn? (lambda () (lambda-exp 'lambda (var-exp 'lambda))))
    (check-exn exn? (lambda () (lambda-exp 'a (var-exp 'lambda))))
    (check-exn exn? (lambda () (lambda-exp 'lambda (var-exp 'a))))
))

(exit (run-tests eopl-2.23-tests))
