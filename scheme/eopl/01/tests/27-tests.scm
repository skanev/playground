(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../27.scm")

(define eopl-1.27-tests
  (test-suite
    "Tests for EOPL exercise 1.27"

    (check-equal? (flatten '(a b c))
                  '(a b c))
    (check-equal? (flatten '((a) () (b ()) () (c)))
                  '(a b c))
    (check-equal? (flatten '((a b) c (((d)) e)))
                  '(a b c d e))
    (check-equal? (flatten '(a b (() (c))))
                  '(a b c))
))

(exit (run-tests eopl-1.27-tests))
