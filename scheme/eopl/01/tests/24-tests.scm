(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../24.scm")

(define eopl-1.24-tests
  (test-suite
    "Tests for EOPL exercise 1.24"

    (check-equal? (every? number? '(a b c 3 e))
                  #f)
    (check-equal? (every? number? '(1 2 3 5 4))
                  #t)
    (check-equal? (every? number? '())
                  #t)
))

(exit (run-tests eopl-1.24-tests))
