(require rackunit rackunit/text-ui)
(load "../61.scm")

(define sicp-2.61-tests
  (test-suite
    "Tests for SICP exercise 2.61"

    (check-equal? (adjoin-set 0 '()) '(0))

    (check-equal? (adjoin-set 0 '(1 3 5)) '(0 1 3 5))
    (check-equal? (adjoin-set 1 '(1 3 5)) '(1 3 5))
    (check-equal? (adjoin-set 2 '(1 3 5)) '(1 2 3 5))
    (check-equal? (adjoin-set 3 '(1 3 5)) '(1 3 5))
    (check-equal? (adjoin-set 4 '(1 3 5)) '(1 3 4 5))
    (check-equal? (adjoin-set 5 '(1 3 5)) '(1 3 5))
    (check-equal? (adjoin-set 6 '(1 3 5)) '(1 3 5 6))
    (check-equal? (adjoin-set 7 '(1 3 5)) '(1 3 5 7))
))

(run-tests sicp-2.61-tests)
