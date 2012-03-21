(require rackunit rackunit/text-ui)
(load "../01.scm")

(define sicp-2.01-tests
  (test-suite
    "Tests for SICP exercise 2.01"

    (check-equal? (numer (make-rat 1 2)) 1)
    (check-equal? (denom (make-rat 1 2)) 2)

    (check-equal? (numer (make-rat -1 2)) -1)
    (check-equal? (denom (make-rat -1 2)) 2)

    (check-equal? (numer (make-rat 1 -2)) -1)
    (check-equal? (denom (make-rat 1 -2)) 2)

    (check-equal? (numer (make-rat -1 -2)) 1)
    (check-equal? (denom (make-rat -1 -2)) 2)

    (check-equal? (numer (make-rat -2 -4)) 1)
    (check-equal? (denom (make-rat -2 -4)) 2)

    (check-equal? (numer (make-rat 2 4)) 1)
    (check-equal? (denom (make-rat 2 4)) 2)
))

(run-tests sicp-2.01-tests)
