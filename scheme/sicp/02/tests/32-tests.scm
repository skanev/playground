(require rackunit rackunit/text-ui)
(load "../32.scm")

(define sicp-2.32-tests
  (test-suite
    "Tests for SICP exercise 2.32"

    (check-equal? (subsets '())
                  '(()))
    (check-equal? (subsets '(1))
                  '(() (1)))
    (check-equal? (subsets '(1 2))
                  '(() (2) (1) (1 2)))
    (check-equal? (subsets '(1 2 3))
                  '(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)))
))

(run-tests sicp-2.32-tests)
