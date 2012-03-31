(require rackunit rackunit/text-ui)
(load "../39.scm")

(define sicp-2.39-tests
  (test-suite
    "Tests for SICP exercise 2.39"

    (check-equal? (reverse-r '(1 2 3 4 5)) '(5 4 3 2 1))
    (check-equal? (reverse-l '(1 2 3 4 5)) '(5 4 3 2 1))
))

(run-tests sicp-2.39-tests)
