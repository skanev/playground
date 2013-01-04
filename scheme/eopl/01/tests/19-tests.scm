(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../19.scm")

(define eopl-1.19-tests
  (test-suite
    "Tests for EOPL exercise 1.19"

    (check-equal? (list-set '(a b c d) 2 '(1 2))
                  '(a b (1 2) d))
    (check-equal? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
                  '(1 5 10))
))

(exit (run-tests eopl-1.19-tests))
