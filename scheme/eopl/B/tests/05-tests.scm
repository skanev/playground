(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../05.scm")

(define eopl-B.05-tests
  (test-suite
    "Tests for EOPL exercise B.05"

    (check-equal? (value-of "-1" '()) -1)
    (check-equal? (value-of "3*-2" '()) -6)
    (check-equal? (value-of "-(x + y)" '((x 2) (y 3))) -5)
))

(exit (run-tests eopl-B.05-tests))
