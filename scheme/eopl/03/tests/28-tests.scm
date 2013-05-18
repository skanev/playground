(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../28.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.28-tests
  (test-suite
    "Tests for EOPL exercise 3.28"

    (check-equal? (run "let a = 3
                        in let p = proc (x) -(x, a)
                           in let a = 5
                              in -(a, (p 2))")
                  8)))

(exit (run-tests eopl-3.28-tests))
