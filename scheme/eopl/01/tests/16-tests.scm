(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../16.scm")

(define eopl-1.16-tests
  (test-suite
    "Tests for EOPL exercise 1.16"

    (check-equal? (invert '((a 1) (a 2) (1 b) (2 b)))
                  '((1 a) (2 a) (b 1) (b 2)))
))

(exit (run-tests eopl-1.16-tests))
