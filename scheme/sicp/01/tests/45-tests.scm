(require rackunit rackunit/text-ui)
(load "../45.scm")

(define sicp-1.45-tests
  (test-suite
    "Tests for SICP exercise 1.45"

    (check-= (nth-root 2 4) 2 0.000001)
    (check-= (nth-root 3 8) 2 0.000001)
    (check-= (nth-root 4 16) 2 0.000001)
    (check-= (nth-root 5 32) 2 0.000001)
    (check-= (nth-root 8 256) 2 0.000001)
    (check-= (nth-root 9 512) 2 0.000001)
))

(run-tests sicp-1.45-tests)
