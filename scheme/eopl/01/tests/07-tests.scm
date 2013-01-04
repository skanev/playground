(require rackunit rackunit/text-ui)
(require eopl)
(load-relative "../07.scm")

(define eopl-1.07-tests
  (test-suite
    "Tests for EOPL exercise 1.07"

    (check-equal? (nth-element '(a b c) 0) 'a)
    (check-equal? (nth-element '(a b c) 1) 'b)
    (check-equal? (nth-element '(a b c) 2) 'c)

    (check-exn (regexp "\\(a b c\\) does not have 8 elements.")
               (lambda () (nth-element '(a b c) 8)))
))

(exit (run-tests eopl-1.07-tests))
