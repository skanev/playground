(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../04.scm")

(define eopl-B.04-tests
  (test-suite
    "Tests for EOPL exercise B.04"

    (check-equal? (value-of "1 + x" '((x 1))) 2)
    (check-equal? (value-of "x * y + z" '((x 2) (y 3) (z 5))) 11)
    (check-equal? (value-of "x * (y + z)" '((x 2) (y 3) (z 5))) 16)
))

(exit (run-tests eopl-B.04-tests))
