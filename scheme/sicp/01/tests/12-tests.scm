(require rackunit rackunit/text-ui)
(load "../12.scm")

(define sicp-1.12-tests
  (test-suite
    "Tests for SICP exercise 1.12"

    (check-equal? (binom 1 1) 1)
    (check-equal? (binom 2 1) 1)
    (check-equal? (binom 2 2) 1)
    (check-equal? (binom 3 1) 1)
    (check-equal? (binom 3 2) 2)
    (check-equal? (binom 3 3) 1)
    (check-equal? (binom 4 1) 1)
    (check-equal? (binom 4 2) 3)
    (check-equal? (binom 4 3) 3)
    (check-equal? (binom 4 4) 1)
    (check-equal? (binom 5 1) 1)
    (check-equal? (binom 5 2) 4)
    (check-equal? (binom 5 3) 6)
    (check-equal? (binom 5 4) 4)
    (check-equal? (binom 5 5) 1)
))

(run-tests sicp-1.12-tests)
