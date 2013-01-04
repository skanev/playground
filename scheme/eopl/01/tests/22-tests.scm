(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../22.scm")

(define eopl-1.22-tests
  (test-suite
    "Tests for EOPL exercise 1.22"

    (check-equal? (filter-in number? '(a 2 (1 3) b 7))
                  '(2 7))
    (check-equal? (filter-in symbol? '(a (b c) 17 foo))
                  '(a foo))
))

(exit (run-tests eopl-1.22-tests))
