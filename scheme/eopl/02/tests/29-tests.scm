(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../29.scm")

(define eopl-2.29-tests
  (test-suite
    "Tests for EOPL exercise 2.29"

    (check-equal? (parse 'x)
                  (var-exp 'x))
    (check-equal? (parse '(lambda (a b c) a))
                  (lambda-exp '(a b c) (var-exp 'a)))
    (check-equal? (parse '((lambda (a b) a) x y))
                  (app-exp (lambda-exp '(a b) (var-exp 'a))
                           (list (var-exp 'x)
                                 (var-exp 'y))))
))

(exit (run-tests eopl-2.29-tests))
