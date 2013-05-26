(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../33.scm")
(load-relative "helpers/letrec.scm")

(define eopl-3.33-tests
  (test-suite
    "Tests for EOPL exercise 3.33"

    (check-equal? (run "letrec
                          even(x, t, f) = if zero?(x) then t else (odd -(x, 1) t f)
                          odd(x, t, f)  = if zero?(x) then f else (even -(x, 1) t f)
                        in (odd 13 101 100)")
                  101)
))

(exit (run-tests eopl-3.33-tests))
