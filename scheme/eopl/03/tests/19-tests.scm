(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../19.scm")
(load-relative "helpers/proc.scm")

(define eopl-3.19-tests
  (test-suite
    "Tests for EOPL exercise 3.19"

    (check-equal? (run "letproc dec (x) = -(x, 1)
                                in (dec 2)")
                  1)

    (check-equal? (run "let a = 10
                            in letproc augment (x) = -(x, a)
                                       in let a = 20
                                              in (augment a)")
                  10)
))

(exit (run-tests eopl-3.19-tests))
