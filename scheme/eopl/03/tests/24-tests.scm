(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../24.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.24-tests
  (test-suite
    "Tests for EOPL exercise 3.24"

    (check-equal? (run the-program) 0)
))

(exit (run-tests eopl-3.24-tests))
