(require rackunit rackunit/text-ui)
(load "../34.scm")

(define sicp-2.34-tests
  (test-suite
    "Tests for SICP exercise 2.34"

    (check-equal? (horner-eval 2 (list 1 3 0 5 0 1)) 79)
))

(run-tests sicp-2.34-tests)
