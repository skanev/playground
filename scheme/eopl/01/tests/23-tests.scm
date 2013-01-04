(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../23.scm")

(define eopl-1.23-tests
  (test-suite
    "Tests for EOPL exercise 1.23"

    (check-equal? (list-index number? '(a 2 (1 3) b 7))
                  1)
    (check-equal? (list-index symbol? '(a (b c) 17 foo))
                  0)
    (check-equal? (list-index symbol? '(1 2 (a b) 3))
                  #f)
))

(exit (run-tests eopl-1.23-tests))
