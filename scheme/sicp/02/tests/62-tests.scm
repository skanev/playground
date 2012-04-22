(require rackunit rackunit/text-ui)
(load "../62.scm")

(define sicp-2.62-tests
  (test-suite
    "Tests for SICP exercise 2.62"

    (check-equal? (union-set '() '()) '())
    (check-equal? (union-set '(1) '()) '(1))
    (check-equal? (union-set '() '(1)) '(1))

    (check-equal? (union-set '(1) '(2)) '(1 2))
    (check-equal? (union-set '(2) '(1)) '(1 2))

    (check-equal? (union-set '(1 3) '(2)) '(1 2 3))
    (check-equal? (union-set '(1) '(2 3)) '(1 2 3))
    (check-equal? (union-set '(2) '(1 3)) '(1 2 3))
    (check-equal? (union-set '(1 2) '(3)) '(1 2 3))

    (check-equal? (union-set '(1 2) '(1 3)) '(1 2 3))
    (check-equal? (union-set '(1 2 3) '(1 2)) '(1 2 3))
    (check-equal? (union-set '(1 2) '(1 2 3)) '(1 2 3))

    (check-equal? (union-set '(1 3 5 7 9) '(2 4 6 8 10)) '(1 2 3 4 5 6 7 8 9 10))
))

(run-tests sicp-2.62-tests)
