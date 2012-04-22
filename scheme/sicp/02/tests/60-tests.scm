(require rackunit rackunit/text-ui)
(load "../60.scm")

(define sicp-2.60-tests
  (test-suite
    "Tests for SICP exercise 2.60"

    (check-true (element-of-set? 1 '(2 3 2 1 3 2 2)))
    (check-true (element-of-set? 2 '(2 3 2 1 3 2 2)))
    (check-true (element-of-set? 3 '(2 3 2 1 3 2 2)))
    (check-false (element-of-set? 4 '(2 3 2 1 3 2 2)))

    (check-equal? (adjoin-set '1 '()) '(1))
    (check-equal? (adjoin-set '1 '(2 3)) '(1 2 3))
    (check-equal? (adjoin-set '2 '(3 2 1)) '(2 3 2 1))

    (check-equal? (union-set '(1 2) '(3 4)) '(1 2 3 4))
    (check-equal? (union-set '(1 2) '(1 3)) '(1 2 1 3))

    (check-equal? (intersection-set '(1 2 3) '(2 3 4)) '(2 3))
    (check-equal? (intersection-set '(1 2 2 3 3) '(2 3 3 4 4 4)) '(2 2 3 3))
    (check-equal? (intersection-set '(1 2 3) '(2 2 2 3 3 4 4 4)) '(2 3))
))

(run-tests sicp-2.60-tests)
