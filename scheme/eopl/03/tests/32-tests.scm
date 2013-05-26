(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../32.scm")
(load-relative "helpers/letrec.scm")

(define eopl-3.32-tests
  (test-suite
    "Tests for EOPL exercise 3.32"

    (check-equal? (run "letrec
                          even(x) = if zero?(x) then 1 else (odd -(x, 1))
                          odd(x)  = if zero?(x) then 0 else (even -(x, 1))
                        in (odd 13)")
                  1)
))

(exit (run-tests eopl-3.32-tests))
