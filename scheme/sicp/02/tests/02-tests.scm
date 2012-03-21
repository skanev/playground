(require rackunit rackunit/text-ui)
(load "../02.scm")

(define sicp-2.02-tests
  (test-suite
    "Tests for SICP exercise 2.02"

    (check-equal? (x-point (make-point 1 2)) 1)
    (check-equal? (y-point (make-point 1 2)) 2)

    (check-equal? (start-segment (make-segment (make-point 1 2) (make-point 3 4)))
                  (make-point 1 2))
    (check-equal? (end-segment (make-segment (make-point 1 2) (make-point 3 4)))
                  (make-point 3 4))

    (check-equal? (midpoint-segment (make-segment (make-point 0 10) (make-point 10 0)))
                  (make-point 5 5))
    (check-equal? (midpoint-segment (make-segment (make-point 0 0) (make-point 0 10)))
                  (make-point 0 5))
    (check-equal? (midpoint-segment (make-segment (make-point 10 0) (make-point 0 0)))
                  (make-point 5 0))
))

(run-tests sicp-2.02-tests)
