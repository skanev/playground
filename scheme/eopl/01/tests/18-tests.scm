(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../18.scm")

(define eopl-1.18-tests
  (test-suite
    "Tests for EOPL exercise 1.18"

    (check-equal? (swapper 'a 'd '(a b c d))
                  '(d b c a))
    (check-equal? (swapper 'a 'd '(a d () c d))
                  '(d a () c a))
    (check-equal? (swapper 'x 'y '((x) y (z (x))))
                  '((y) x (z (y))))
))

(exit (run-tests eopl-1.18-tests))
