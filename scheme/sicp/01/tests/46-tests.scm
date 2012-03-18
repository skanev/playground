(require rackunit rackunit/text-ui)
(load "../46.scm")

(define sicp-1.46-tests
  (test-suite
    "Tests for SICP exercise 1.46"

    (check-= (sqrt 9) 3 0.0000001)
    (check-= (sqrt 256) 16 0.0000001)

    (check-= (fixed-point cos 1.0) 0.739084 0.00001)
))

(run-tests sicp-1.46-tests)
