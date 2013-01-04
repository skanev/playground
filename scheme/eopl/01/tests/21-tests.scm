(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../21.scm")

(define eopl-1.21-tests
  (test-suite
    "Tests for EOPL exercise 1.21"

    (check-equal? (product '(a b c) '(x y))
                  '((a x) (a y) (b x) (b y) (c x) (c y)))
))

(exit (run-tests eopl-1.21-tests))
