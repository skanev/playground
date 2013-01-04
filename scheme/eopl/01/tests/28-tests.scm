(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../28.scm")

(define eopl-1.28-tests
  (test-suite
    "Tests for EOPL exercise 1.28"

    (check-equal? (merge '(1 4) '(1 2 8))
                  '(1 1 2 4 8))
    (check-equal? (merge '(35 62 81 90 91) '(3 83 85 90))
                  '(3 35 62 81 83 85 90 90 91))
))

(exit (run-tests eopl-1.28-tests))
