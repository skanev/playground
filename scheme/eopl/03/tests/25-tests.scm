(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../25.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.25-tests
  (test-suite
    "Tests for EOPL exercise 3.25"

    (check-equal? (run the-example-program) 12)
))

(exit (run-tests eopl-3.25-tests))
