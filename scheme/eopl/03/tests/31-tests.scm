(require rackunit rackunit/text-ui)
(load-relative "../../support/eopl.scm")
(load-relative "../31.scm")
(load-relative "helpers/letrec.scm")

(define eopl-3.31-tests
  (test-suite
    "Tests for EOPL exercise 3.31"

    (check-equal? (run "let one = proc () 1
                            in (one)")
                  1)
    (check-equal? (run "let minus = proc (x, y) -(x, y)
                            in (minus 7 2)")
                  5)

    (check-equal? (run "letrec double(x)
                                = if zero?(x) then 0 else -((double -(x, 1)), -(0, 2))
                        in (double 6)")
                  12)

    (check-equal? (run "let fib = proc (n)
                          letrec iter(n, a, b) = if zero?(-(n, 1))
                                                 then a
                                                 else (iter -(n, 1) -(a, -(0, b)) a)
                          in (iter n 1 0)
                        in (fib 10)")
                        55)
))

(exit (run-tests eopl-3.31-tests))
