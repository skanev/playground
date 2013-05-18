(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../21.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.21-tests
  (test-suite
    "Tests for EOPL exercise 3.21"

    (check-equal? (run "let one = proc () 1
                            in (one)")
                  1)
    (check-equal? (run "let minus = proc (x, y) -(x, y)
                            in (minus 7 2)")
                  5)
))

(exit (run-tests eopl-3.21-tests))
