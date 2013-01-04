(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../29.scm")

(define eopl-1.29-tests
  (test-suite
    "Tests for EOPL exercise 1.29"

    (check-equal? (sort '(8 2 5 2 3))
                  '(2 2 3 5 8))
))

(exit (run-tests eopl-1.29-tests))
