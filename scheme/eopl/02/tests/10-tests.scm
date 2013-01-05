(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../10.scm")

(define eopl-2.10-tests
  (test-suite
    "Tests for EOPL exercise 2.10"

    (check-equal? (apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'a)
                  1)
    (check-equal? (apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'b)
                  2)
    (check-equal? (apply-env (extend-env* '(a b c) '(1 2 3) (empty-env)) 'c)
                  3)
))

(exit (run-tests eopl-2.10-tests))
