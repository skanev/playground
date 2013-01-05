(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../36.scm")

(define eopl-1.36-tests
  (test-suite
    "Tests for EOPL exercise 1.36"

    (check-equal? (number-elements '(a b c d))
                  '((0 a) (1 b) (2 c) (3 d)))
))

(exit (run-tests eopl-1.36-tests))
