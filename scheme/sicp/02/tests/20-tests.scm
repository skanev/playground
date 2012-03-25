(require rackunit rackunit/text-ui)
(load "../20.scm")

(define sicp-2.20-tests
  (test-suite
    "Tests for SICP exercise 2.20"

    (check-equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
    (check-equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))
))

(run-tests sicp-2.20-tests)
