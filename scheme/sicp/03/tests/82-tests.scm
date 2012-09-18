(require rackunit rackunit/text-ui)
(load "../82.scm")

(define sicp-3.82-tests
  (test-suite
    "Tests for SICP exercise 3.82"

    (check-= (estimate-pi 100000) 3.14 0.01)
))

(run-tests sicp-3.82-tests)
