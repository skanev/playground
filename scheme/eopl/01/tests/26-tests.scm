(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../26.scm")

(define eopl-1.26-tests
  (test-suite
    "Tests for EOPL exercise 1.26"

    (check-equal? (up '((1 2) (3 4)))
                  '(1 2 3 4))
    (check-equal? (up '((x (y)) z))
                  '(x (y) z))
))

(exit (run-tests eopl-1.26-tests))
