(require rackunit rackunit/text-ui)
(load "../08.scm")

(define sicp-1.08-tests
  (test-suite
    "Tests for SICP exercise 1.08"

    (check-= (cube-root 8) 2 0.00001)
    (check-= (cube-root 27) 3 0.00001)
    (check-= (cube-root 1000) 10 0.00001)
))

(run-tests sicp-1.08-tests)
