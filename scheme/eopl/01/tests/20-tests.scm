(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../20.scm")

(define eopl-1.20-tests
  (test-suite
    "Tests for EOPL exercise 1.20"

    (check-equal? (count-occurences 'x '((f x) y (((x z) x))))
                  3)
    (check-equal? (count-occurences 'x '((f x) y (((x z) () x))))
                  3)
    (check-equal? (count-occurences 'w '((f x) y (((x z) x))))
                  0)
))

(exit (run-tests eopl-1.20-tests))
