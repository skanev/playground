(require rackunit rackunit/text-ui)
(load "../28.scm")

(define sicp-2.28-tests
  (test-suite
    "Tests for SICP exercise 2.28"

    (check-equal? (fringe '((1 2) (3 4))) '(1 2 3 4))
    (check-equal? (fringe '(((1 2) (3 4)) ((1 2) (3 4)))) '(1 2 3 4 1 2 3 4))
    (check-equal? (fringe '(((1 2) 3 4) 5)) '(1 2 3 4 5))
    (check-equal? (fringe '(1 (2 (3 4) 5 (6)) 7)) '(1 2 3 4 5 6 7))
))

(run-tests sicp-2.28-tests)
