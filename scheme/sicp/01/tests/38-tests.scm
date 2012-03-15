(require rackunit rackunit/text-ui)
(load "../38.scm")

(define sicp-1.38-tests
  (test-suite
    "Tests for SICP exercise 1.38"

    (check-= (approximate-pi) 0.71828183 0.00000001)
))

(run-tests sicp-1.38-tests)
