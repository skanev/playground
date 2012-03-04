(require rackunit rackunit/text-ui)
(load "../16.scm")

(define sicp-1.16-tests
  (test-suite
    "Tests for SICP exercise 1.16"

    (check-equal? (fast-expt 2 0) 1)
    (check-equal? (fast-expt 2 1) 2)
    (check-equal? (fast-expt 2 2) 4)
    (check-equal? (fast-expt 2 3) 8)
    (check-equal? (fast-expt 2 4) 16)
    (check-equal? (fast-expt 2 5) 32)
    (check-equal? (fast-expt 2 6) 64)
    (check-equal? (fast-expt 2 7) 128)
    (check-equal? (fast-expt 2 8) 256)

    (check-equal? (fast-expt 3 0) 1)
    (check-equal? (fast-expt 3 1) 3)
    (check-equal? (fast-expt 3 2) 9)
    (check-equal? (fast-expt 3 3) 27)
    (check-equal? (fast-expt 3 4) 81)
    (check-equal? (fast-expt 3 5) 243)
))

(run-tests sicp-1.16-tests)
