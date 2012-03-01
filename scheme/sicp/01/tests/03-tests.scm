(require rackunit rackunit/text-ui)
(load "../03.scm")

(define sicp-1.03-tests
  (test-suite
    "Tests for SICP exercise 1.03"

    (check-equal? (sum-of-two-largest-squares 2 3 4) 25)
    (check-equal? (sum-of-two-largest-squares 2 4 3) 25)
    (check-equal? (sum-of-two-largest-squares 3 4 2) 25)
    (check-equal? (sum-of-two-largest-squares 3 2 4) 25)
    (check-equal? (sum-of-two-largest-squares 4 2 3) 25)
    (check-equal? (sum-of-two-largest-squares 4 3 2) 25)

    (check-equal? (sum-of-two-largest-squares 3 3 4) 25)
    (check-equal? (sum-of-two-largest-squares 3 4 3) 25)
    (check-equal? (sum-of-two-largest-squares 4 3 3) 25)

    (check-equal? (sum-of-two-largest-squares 3 3 3) 18)
))

(run-tests sicp-1.03-tests)
