(require rackunit rackunit/text-ui)
(load "../05.scm")

(define sicp-3.05-tests
  (test-suite
    "Tests for SICP exercise 3.05"

    (check-= (estimate-pi) 3.14 0.01)
))

(run-tests sicp-3.05-tests)
