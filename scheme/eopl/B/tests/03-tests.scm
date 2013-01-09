(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../03.scm")

(define eopl-B.03-tests
  (test-suite
    "Tests for EOPL exercise B.03"

    (check-equal? (value-of "1 + 2") 3)
    (check-equal? (value-of "3 - 2") 1)
    (check-equal? (value-of "3 - 2 - 1") 0)
    (check-equal? (value-of "3 - 2 - 1 + 7") 7)
    (check-equal? (value-of "3 * 4 / 2 * 5") 30)
    (check-equal? (value-of "3 * 4 / 2 * 5 + 1") 31)
))

(exit (run-tests eopl-B.03-tests))
