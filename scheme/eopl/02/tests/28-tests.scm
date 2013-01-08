(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../28.scm")

(define eopl-2.28-tests
  (test-suite
    "Tests for EOPL exercise 2.28"

    (check-equal? (unparse (var-exp 'a))
                  "a")
    (check-equal? (unparse (lambda-exp 'x (app-exp (var-exp 'x) (var-exp 'y))))
                  "(lambda (x) (x y))")
    (check-equal? (unparse (app-exp (lambda-exp 'x (var-exp 'x))
                                    (app-exp (var-exp 'a) (var-exp 'b))))
                  "((lambda (x) x) (a b))")
))

(exit (run-tests eopl-2.28-tests))
