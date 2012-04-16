(require rackunit rackunit/text-ui)
(load "../40.scm")

(define sicp-2.40-tests
  (test-suite
    "Tests for SICP exercise 2.40"

    (check-equal? (enumerate-interval 1 5) '(1 2 3 4 5))

    (check-equal? (unique-pairs 2) '((1 2)))
    (check-equal? (unique-pairs 3) '((1 2) (1 3) (2 3)))
    (check-equal? (unique-pairs 4) '((1 2) (1 3) (1 4) (2 3) (2 4) (3 4)))

    (check-equal? (prime-sum-pairs 6) '((1 2) (1 4) (1 6) (2 3) (2 5) (3 4) (5 6)))
))

(run-tests sicp-2.40-tests)
