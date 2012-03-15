(require rackunit rackunit/text-ui)
(load "../39.scm")

(define sicp-1.39-tests
  (test-suite
    "Tests for SICP exercise 1.39"

    (check-= (tan-cf 0 100) 0 0.00001)
    (check-= (tan-cf 1.0 100) 1.55740 0.00001)
    (check-= (tan-cf 2.0 100) -2.18503 0.00001)
    (check-= (tan-cf 3.141592 100) 0.0 0.00001)
))

(run-tests sicp-1.39-tests)
