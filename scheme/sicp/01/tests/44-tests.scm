(require rackunit rackunit/text-ui)
(load "../44.scm")

(define sicp-1.44-tests
  (test-suite
    "Tests for SICP exercise 1.44"

    ; A rather flaky way of testing it
    (check-not-equal? ((smoothed abs) 0) 0)
    (check-= ((smoothed abs) 0) 0 0.00001)

    (check-not-equal? ((n-fold-smoothed abs 4) 0) 0)
    (check-= ((n-fold-smoothed abs 4) 0) 0 0.00001)
))

(run-tests sicp-1.44-tests)
