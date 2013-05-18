(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../22.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.22-tests
  (test-suite
    "Tests for EOPL exercise 3.22"

    (check-equal? (run "let add = proc (x, y) -(x, -(0, y))
                            in add(2, 3)")
                  5)
))

(exit (run-tests eopl-3.22-tests))
