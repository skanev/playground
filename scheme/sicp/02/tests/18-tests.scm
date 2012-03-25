(require rackunit rackunit/text-ui)
(load "../18.scm")

(define sicp-2.18-tests
  (test-suite
    "Tests for SICP exercise 2.18"

    (check-equal? (reverse (list 1 4 9 16 25)) (list 25 16 9 4 1))
    (check-equal? (reverse (list 1)) (list 1))
    (check-equal? (reverse '()) '())
))

(run-tests sicp-2.18-tests)
