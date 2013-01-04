(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../09.scm")

(define eopl-1.09-tests
  (test-suite
    "Tests for EOPL exercise 1.09"

    (check-equal? (remove 'a '(a b a c a d a))
                  '(b c d))
))

(exit (run-tests eopl-1.09-tests))
