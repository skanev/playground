(require rackunit rackunit/text-ui)
(load "../64.scm")

(define sicp-3.64-tests
  (test-suite
    "Tests for SICP exercise 3.64"

    (check-= (sqrt-tolerance 2 0.0000000001) 1.41421356237 0.0000000001)
))

(run-tests sicp-3.64-tests)
