(require rackunit rackunit/text-ui)
(load "../28.scm")

(define sicp-1.28-tests
  (test-suite
    "Tests for SICP exercise 1.28"

    (check-true (fast-prime? 11 3))
    (check-true (fast-prime? 101 3))
    (check-true (fast-prime? 1009 3))
    (check-true (fast-prime? 1013 3))
    (check-true (fast-prime? 1019 3))
    (check-true (fast-prime? 10007 3))
    (check-true (fast-prime? 10009 3))
    (check-true (fast-prime? 10037 3))
    (check-true (fast-prime? 10037 3))

    (check-false (fast-prime? 561 3))
    (check-false (fast-prime? 1105 3))
    (check-false (fast-prime? 1729 3))
    (check-false (fast-prime? 2465 3))
    (check-false (fast-prime? 2821 3))
    (check-false (fast-prime? 6601 3))
))

(run-tests sicp-1.28-tests)
