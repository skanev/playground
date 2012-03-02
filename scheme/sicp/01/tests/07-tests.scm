(require rackunit rackunit/text-ui)
(load "../07.scm")

(define sicp-1.07-tests
  (test-suite
    "Tests for SICP exercise 1.07"

    (check-= (sqrt 4e-8) 2e-4 1e-16)
    (check-= (* (sqrt 10e+48) (sqrt 10e+48)) 10e+48 10e+33)
))

(run-tests sicp-1.07-tests)
