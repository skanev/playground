(require rackunit rackunit/text-ui)
(load "../40.scm")

(define sicp-1.40-tests
  (test-suite
    "Tests for SICP exercise 1.40"

    (check-= (newtons-method (cubic 0 0 -1) 2.0) 1.0 0.00001)
    (check-= (newtons-method (cubic 0 0 -27) 2.0) 3.0 0.00001)
))

(run-tests sicp-1.40-tests)
