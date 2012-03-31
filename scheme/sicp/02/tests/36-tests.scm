(require rackunit rackunit/text-ui)
(load "../36.scm")

(define sicp-2.36-tests
  (test-suite
    "Tests for SICP exercise 2.36"

    (check-equal? (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) '(22 26 30))
))

(run-tests sicp-2.36-tests)
