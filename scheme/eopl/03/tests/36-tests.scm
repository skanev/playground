(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../36.scm")
(load-relative "helpers/letrec.scm")

(define eopl-3.36-tests
  (test-suite
    "Tests for EOPL exercise 3.36"

    (check-equal? (run "letrec
                          even(x) = if zero?(x) then 1 else (odd -(x, 1))
                          odd(x)  = if zero?(x) then 0 else (even -(x, 1))
                        in (odd 13)")
                  1)
))

(exit (run-tests eopl-3.36-tests))
