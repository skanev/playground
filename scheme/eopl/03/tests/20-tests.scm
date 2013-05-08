(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../20.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.20-tests
  (test-suite
    "Tests for EOPL exercise 3.20"

    (check-equal? (run two-plus-three) 5)
))

(exit (run-tests eopl-3.20-tests))
