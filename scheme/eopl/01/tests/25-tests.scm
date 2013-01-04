(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../25.scm")

(define eopl-1.25-tests
  (test-suite
    "Tests for EOPL exercise 1.25"

    (check-equal? (exists? number? '(a b c 3 e))
                  #t)
    (check-equal? (exists? number? '(a b c d e))
                  #f)
    (check-equal? (exists? number? '())
                  #f)
))

(exit (run-tests eopl-1.25-tests))
