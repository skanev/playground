(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../23.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.23-tests
  (test-suite
    "Tests for EOPL exercise 3.23"

    (check-equal? (run the-given-program) 12)
    (check-equal? (run factorial-5-program) 120)
))

(exit (run-tests eopl-3.23-tests))
