(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../37.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.37-tests
  (test-suite
    "Tests for EOPL exercise 3.37"

    (check-equal? (run the-program) 1)
))

(exit (run-tests eopl-3.37-tests))
