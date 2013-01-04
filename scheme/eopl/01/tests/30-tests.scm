(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../30.scm")

(define eopl-1.30-tests
  (test-suite
    "Tests for EOPL exercise 1.30"

    (check-equal? (sort/predicate < '(8 2 5 2 3))
                  '(2 2 3 5 8))
    (check-equal? (sort/predicate > '(8 2 5 2 3))
                  '(8 5 3 2 2))
))

(exit (run-tests eopl-1.30-tests))
