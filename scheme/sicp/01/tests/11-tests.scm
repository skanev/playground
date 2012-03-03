(require rackunit rackunit/text-ui)
(load "../11.scm")

(define sicp-1.11-tests
  (test-suite
    "Tests for SICP exercise 1.11"

    (check-equal? (f 0) 0)
    (check-equal? (f 1) 1)
    (check-equal? (f 2) 2)
    (check-equal? (f 3) 4)
    (check-equal? (f 4) 11)
    (check-equal? (f 5) 25)
    (check-equal? (f 6) 59)

    (check-equal? (f-iter 0) 0)
    (check-equal? (f-iter 1) 1)
    (check-equal? (f-iter 2) 2)
    (check-equal? (f-iter 3) 4)
    (check-equal? (f-iter 4) 11)
    (check-equal? (f-iter 5) 25)
    (check-equal? (f-iter 6) 59)
))

(run-tests sicp-1.11-tests)
