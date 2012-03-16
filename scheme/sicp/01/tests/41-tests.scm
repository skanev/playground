(require rackunit rackunit/text-ui)
(load "../41.scm")

(define sicp-1.41-tests
  (test-suite
    "Tests for SICP exercise 1.41"

    (check-equal? (((double (double double)) inc) 5) 21)
))

(run-tests sicp-1.41-tests)
