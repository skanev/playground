(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../31.scm")

(define eopl-2.31-tests
  (test-suite
    "Tests for EOPL exercise 2.31"

    (check-equal? (parse '(- - 3 2 - 4 - 12 7))
                  (diff-exp (diff-exp (const-exp 3) (const-exp 2))
                            (diff-exp (const-exp 4)
                                      (diff-exp (const-exp 12) (const-exp 7)))))
))

(exit (run-tests eopl-2.31-tests))
